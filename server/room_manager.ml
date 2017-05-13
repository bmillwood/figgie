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

let broadcast_scores t =
  Map.iteri (Game.scores t.game) ~f:(fun ~key:username ~data:score ->
      apply_room_update t { username; event = Player_score score }
    )

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
  >>= fun in_seat ->
  Game.player_join t.game ~username
  >>| fun () ->
  apply_room_update t
    { username
    ; event = Observer_started_playing { in_seat }
    };
  in_seat

let player_join t ~username =
  let updates_r, updates_w = Pipe.create () in
  Updates_manager.subscribe t.room_updates ~username ~updates:updates_w;
  apply_room_update t { username; event = Joined };
  let catch_up =
    let open Protocol.Game_update in
    [ [ Room_snapshot t.room ]
    ; match t.game.phase with
      | Waiting_for_players _ -> []
      | Playing round ->
          [ Some (Broadcast New_round)
          ; Option.map (Map.find round.players username)
              ~f:(fun p -> Hand p.hand)
          ; Some (Market round.market)
          ] |> List.filter_opt
    ] |> List.concat
  in
  List.iter catch_up ~f:(fun update ->
    Pipe.write_without_pushback updates_w update);
  updates_r

let chat t ~username msg =
  Updates_manager.broadcast t.room_updates
    (Broadcast (Chat (username, msg)))

let setup_round t (round : Game.Round.t) =
  don't_wait_for begin
    Clock_ns.at round.end_time
    >>| fun () ->
    let results = Game.end_round t.game round in
    Updates_manager.broadcast t.room_updates
      (Broadcast (Round_over results));
    Map.iter (Lobby.Room.seating t.room) ~f:(fun username ->
        apply_room_update t { username; event = Player_ready false }
      );
    broadcast_scores t
  end;
  Map.iteri round.players ~f:(fun ~key:username ~data:p ->
    Updates_manager.update t.room_updates ~username (Hand p.hand)
  );
  Updates_manager.broadcast t.room_updates
    (Broadcast New_round);
  broadcast_scores t

let player_ready t ~username ~is_ready =
  Result.map (Game.set_ready t.game ~username ~is_ready)
    ~f:(fun started_or_not ->
        apply_room_update t { username; event = Player_ready is_ready };
        match started_or_not with
        | `Started round -> setup_round t round
        | `Still_waiting _wait -> ()
      )

let cancel_all_for_player t ~username ~round =
  Result.map (Game.Round.cancel_orders round ~sender:username)
    ~f:(fun orders ->
        List.iter orders ~f:(fun order ->
            Updates_manager.broadcast t.room_updates
              (Broadcast (Out order)));
        `Ack
      )

let player_disconnected t ~username =
  begin match t.game.phase with
  | Playing round ->
    begin match cancel_all_for_player t ~username ~round with
    | Error `You're_not_playing | Ok `Ack -> ()
    end
  | Waiting_for_players _ ->
    begin match player_ready t ~username ~is_ready:false with
    | Error (`Game_already_in_progress | `You're_not_playing)
    | Ok () -> ()
    end
  end;
  apply_room_update t { username; event = Disconnected }

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
        match room.game.phase with
        | Waiting_for_players _ -> return (Error `Game_not_in_progress)
        | Playing round -> f ~username ~room ~round query
      )
  in
  [ implement Protocol.Start_playing.rpc (fun ~room ~username in_seat ->
        return (start_playing room ~username ~in_seat)
      )
  ; implement Protocol.Is_ready.rpc (fun ~room ~username is_ready ->
        return (player_ready room ~username ~is_ready)
      )
  ; during_game Protocol.Time_remaining.rpc
      (fun ~username:_ ~room:_ ~round () ->
        let span = Time_ns.diff round.end_time (Time_ns.now ()) in
        return (Ok span)
      )
  ; during_game Protocol.Get_update.rpc
      (fun ~username ~room ~round which ->
        begin match which with
        | Hand ->
          Result.iter (Game.Round.get_hand round ~username) ~f:(fun hand ->
              Updates_manager.update room.room_updates ~username (Hand hand))
        | Market ->
          Updates_manager.update room.room_updates ~username
            (Market round.market)
        end;
        return (Ok ())
      )
  ; during_game Protocol.Order.rpc
      (fun ~username ~room ~round order ->
        match Game.Round.add_order round ~order ~sender:username with
        | (Error _) as e -> return e
        | Ok exec ->
          Updates_manager.broadcast room.room_updates
            (Broadcast (Exec (order, exec)));
          broadcast_scores room;
          return (Ok `Ack)
      )
  ; during_game Protocol.Cancel.rpc
      (fun ~username ~room ~round id ->
        match Game.Round.cancel_order round ~id ~sender:username with
        | (Error _) as e -> return e
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
