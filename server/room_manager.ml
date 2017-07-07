open Core
open Async

open Figgie

type t =
  { id : Lobby.Room.Id.t
  ; mutable room : Lobby.Room.t
  ; game : Game.t
  ; lobby_updates : Protocol.Lobby_update.t Updates_manager.t
  ; room_updates  : Protocol.Game_update.t  Updates_manager.t
  }

let create ~game_config ~id ~lobby_updates =
  { id
  ; room = Lobby.Room.empty
  ; game = Game.create ~config:game_config
  ; lobby_updates
  ; room_updates = Updates_manager.create ()
  }

let room_snapshot t = t.room
let is_empty t = Lobby.Room.is_empty t.room

let apply_room_update t update =
  t.room <- Lobby.Room.Update.apply update t.room;
  Updates_manager.broadcast t.room_updates (Broadcast (Room_update update));
  Updates_manager.broadcast t.lobby_updates
    (Lobby_update (Room_update { room_id = t.id; update }))

let start_playing t ~username ~(in_seat : Protocol.Start_playing.query) =
  let open Result.Monad_infix in
  begin match Map.find (Lobby.Room.users t.room) username with
  | Some user ->
    begin match Lobby.User.role user with
    | Player _ -> Error `You're_already_playing
    | Observer _ -> Ok ()
    end
  | None -> Error `Not_in_a_room
  end
  >>= fun () ->
  let seats_to_try =
    match in_seat with
    | Sit_in seat -> [seat]
    | Sit_anywhere -> Lobby.Room.Seat.all
  in
  Result.of_option ~error:`Seat_occupied (
    List.find seats_to_try ~f:(fun seat ->
      not (Map.mem (Lobby.Room.seating t.room) seat)
    )
  )
  >>| fun in_seat ->
  apply_room_update t
    (Player_event
       { username
       ; event = Observer_started_playing { in_seat }
       });
  in_seat

let tell_player_their_hand t round ~username =
  Result.iter (Game.Round.get_hand round ~username) ~f:(fun hand ->
      Updates_manager.update t.room_updates ~username (Hand hand)
    )

let player_join t ~username =
  let updates_r, updates_w = Pipe.create () in
  Updates_manager.subscribe t.room_updates ~username ~updates:updates_w;
  apply_room_update t (Player_event { username; event = Joined });
  Updates_manager.update t.room_updates ~username
    (Room_snapshot (room_snapshot t));
  begin match Game.phase t.game with
  | Waiting_for_players -> ()
  | Playing round ->
    Updates_manager.updates t.room_updates ~username
      [ Broadcast (Room_update Start_playing)
      ; Market (Game.Round.market round)
      ];
    tell_player_their_hand t round ~username
  end;
  updates_r

let chat t ~username msg =
  Updates_manager.broadcast t.room_updates
    (Broadcast (Chat (username, msg)))

let end_round t (round : Game.Round.t) =
  let results = Game.end_round t.game round in
  Updates_manager.broadcast t.room_updates
    (Broadcast (Round_over results));
  apply_room_update t Start_waiting;
  Map.iter2 (Lobby.Room.players t.room) results.positions_this_round
    ~f:(fun ~key:_ ~data:merge ->
        match merge with
        | `Left _ | `Right _ -> ()
        | `Both (player, this_round) ->
          let hand = Partial_hand.create_known this_round.stuff in
          let username = player.username in
          apply_room_update t
            (Player_event { username; event = Player_hand hand });
          let score =
            Market.Price.O.(player.role.score + this_round.cash)
          in
          apply_room_update t
            (Player_event { username; event = Player_score score });
      )

let setup_round t (round : Game.Round.t) =
  don't_wait_for begin
    Clock_ns.at (Game.Round.end_time round)
    >>| fun () ->
    end_round t round
  end;
  apply_room_update t Start_playing;
  Map.iteri (Lobby.Room.players t.room) ~f:(fun ~key:username ~data:player ->
      let unknown_hand =
        Partial_hand.create_unknown Params.num_cards_per_hand
      in
      apply_room_update t
        (Player_event { username; event = Player_hand unknown_hand });
      tell_player_their_hand t round ~username;
      let score =
        Market.Price.O.(player.role.score - Params.pot_per_player)
      in
      apply_room_update t
        (Player_event { username; event = Player_score score })
    )

let player_ready t ~username ~is_ready =
  match Game.phase t.game with
  | Playing _ -> Error `Game_already_started
  | Waiting_for_players ->
    if Lobby.Room.has_player t.room ~username then (
      apply_room_update t
        (Player_event { username; event = Player_ready is_ready });
      if Lobby.Room.is_ready t.room then (
        Game.start_round t.game ~room:t.room
        |> setup_round t
      );
      Ok ()
    ) else (
      Error `You're_not_playing
    )

let cancel_all_for_player t ~username ~round =
  match Game.Round.cancel_orders round ~sender:username with
  | Error `You're_not_playing as e -> e
  | Ok orders ->
    List.iter orders ~f:(fun order ->
        Updates_manager.broadcast t.room_updates
          (Broadcast (Out order)));
    Ok `Ack

let player_disconnected t ~username =
  begin match Game.phase t.game with
  | Playing round ->
    begin match cancel_all_for_player t ~username ~round with
    | Error `You're_not_playing | Ok `Ack -> ()
    end
  | Waiting_for_players ->
    begin match player_ready t ~username ~is_ready:false with
    | Error (`Game_already_started | `You're_not_playing)
    | Ok () -> ()
    end
  end;
  apply_room_update t (Player_event { username; event = Disconnected })

let rpc_implementations =
  let implement rpc f =
    Rpc.Rpc.implement rpc (fun r q ->
        match r with
        | Error (`Not_logged_in | `Not_in_a_room as e) ->
          return (Error e)
        | Ok (t, username) -> f ~room:t ~username q
      )
  in
  let during_game rpc f =
    implement rpc (fun ~room ~username query ->
        match Game.phase room.game with
        | Waiting_for_players -> return (Error `Game_not_in_progress)
        | Playing round -> f ~username ~room ~round query
      )
  in
  [ implement Protocol.Start_playing.rpc (fun ~room ~username in_seat ->
        return (start_playing room ~username ~in_seat)
      )
  ; implement Protocol.Is_ready.rpc (fun ~room ~username is_ready ->
        match player_ready room ~username ~is_ready with
        | Error (`Game_already_started | `You're_not_playing as e) ->
          return (Error e)
        | Ok r -> return (Ok r)
      )
  ; during_game Protocol.Time_remaining.rpc
      (fun ~username:_ ~room:_ ~round () ->
        let span =
          Time_ns.diff (Game.Round.end_time round) (Time_ns.now ())
        in
        return (Ok span)
      )
  ; during_game Protocol.Get_update.rpc
      (fun ~username ~room ~round which ->
        match which with
        | Hand ->
          begin match Game.Round.get_hand round ~username with
          | Error `You're_not_playing as e -> return e
          | Ok hand ->
              Updates_manager.update room.room_updates ~username
                (Hand hand);
              return (Ok ())
          end
        | Market ->
          Updates_manager.update room.room_updates ~username
            (Market (Game.Round.market round));
          return (Ok ())
      )
  ; during_game Protocol.Order.rpc
      (fun ~username ~room ~round order ->
        match Game.Round.add_order round ~order ~sender:username with
        | (Error _) as e -> return e
        | Ok exec ->
          Updates_manager.broadcast room.room_updates (Broadcast (Exec exec));
          let adjust_for_posted_sell =
            let do_nothing _username _hand = None in
            match exec.posted with
            | None -> do_nothing
            | Some posted ->
              Market.Dir.fold posted.dir
                ~buy:do_nothing
                ~sell:(fun username hand ->
                    if Username.equal posted.owner username then (
                      Some (
                        Partial_hand.selling hand
                          ~suit:posted.symbol ~size:posted.size
                      )
                    ) else (
                      None
                    )
                  )
          in
          let updates =
            Map.merge
              (Lobby.Room.players room.room)
              (Market.Exec.position_effect exec)
              ~f:(fun ~key:_ ->
                  let open Lobby.Room.Update.User_event in
                  function
                  | `Left user ->
                    Option.map
                      (adjust_for_posted_sell user.username user.role.hand)
                      ~f:(fun new_hand -> [Player_hand new_hand])
                  | `Right _ -> assert false
                  | `Both (user, pos_diff) ->
                      let score =
                        Market.Price.O.(user.role.score + pos_diff.cash)
                      in
                      let hand =
                        Partial_hand.apply_positions_diff user.role.hand
                          ~diff:pos_diff.stuff
                      in
                      let hand =
                        Option.value ~default:hand
                          (adjust_for_posted_sell user.username hand)
                      in
                      Some
                        [ Player_score score
                        ; Player_hand hand
                        ]
                )
            |> Map.to_alist
            |> List.concat_map ~f:(fun (username, events) ->
                List.map events ~f:(fun event ->
                    Lobby.Room.Update.Player_event { username; event }
                  )
              )
          in
          List.iter updates ~f:(apply_room_update room);
          return (Ok `Ack)
      )
  ; during_game Protocol.Cancel.rpc
      (fun ~username ~room ~round id ->
        match Game.Round.cancel_order round ~id ~sender:username with
        | (Error (`No_such_order | `You're_not_playing)) as e -> return e
        | Ok order ->
          Updates_manager.broadcast room.room_updates
            (Broadcast (Out order));
          return (Ok `Ack)
      )
  ; during_game Protocol.Cancel_all.rpc
      (fun ~username ~room ~round () ->
        return (cancel_all_for_player room ~username ~round)
      )
  ]
