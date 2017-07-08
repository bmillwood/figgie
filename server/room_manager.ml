open Core
open Async

open Figgie

type t =
  { id : Lobby.Room.Id.t
  ; mutable room : Lobby.Room.t
  ; mutable closed : bool
  ; game_config : Game.Config.t
  ; mutable game : Game.t option
  ; lobby_updates : Protocol.Lobby_update.t Updates_manager.t
  ; room_updates  : Protocol.Game_update.t  Updates_manager.t
  }

let create ~game_config ~id ~lobby_updates =
  { id
  ; room = Lobby.Room.empty
  ; closed = false
  ; game_config
  ; game = None
  ; lobby_updates
  ; room_updates = Updates_manager.create ()
  }

let room_snapshot t = t.room
let is_empty t = Lobby.Room.is_empty t.room

let close t =
  Map.iter_keys (Lobby.Room.users t.room) ~f:(fun username ->
      Updates_manager.unsubscribe t.room_updates ~username
    );
  Updates_manager.broadcast t.lobby_updates
    (Lobby_update (Room_closed { room_id = t.id }));
  t.closed <- true

let apply_room_update t update =
  if t.closed then (
    raise_s [%message
      "Applied update to closed room"
        (t.id : Lobby.Room.Id.t)
        (update : Lobby.Room.Update.t)
    ]
  );
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

let tell_player_their_hand t ~round ~username =
  Result.iter (Game.get_hand round ~username) ~f:(fun hand ->
      Updates_manager.update t.room_updates ~username (Hand hand)
    )

let player_join t ~username ~is_bot =
  let updates_r, updates_w = Pipe.create () in
  Updates_manager.subscribe t.room_updates ~username ~updates:updates_w;
  apply_room_update t
    (Player_event { username; event = Joined { is_bot } });
  Updates_manager.update t.room_updates ~username
    (Room_snapshot (room_snapshot t));
  Option.iter t.game ~f:(fun round ->
      Updates_manager.updates t.room_updates ~username
        [ Broadcast (Room_update Start_round)
        ; Market (Game.market round)
        ];
      tell_player_their_hand t ~round ~username
    );
  updates_r

let chat t ~username msg =
  Updates_manager.broadcast t.room_updates
    (Broadcast (Chat (username, msg)))

let end_round t (round : Game.t) =
  let results = Game.results round in
  t.game <- None;
  apply_room_update t (Round_over results)

let setup_round t (round : Game.t) =
  t.game <- Some round;
  don't_wait_for begin
    Clock_ns.at (Game.end_time round)
    >>| fun () ->
    end_round t round
  end;
  apply_room_update t Start_round;
  Map.iter_keys (Lobby.Room.players t.room) ~f:(fun username ->
      tell_player_their_hand t ~round ~username
    )

let player_ready t ~username ~is_ready =
  if Option.is_some t.game then (
    Error `Game_already_started
  ) else (
    if Lobby.Room.has_player t.room ~username then (
      apply_room_update t
        (Player_event { username; event = Player_ready is_ready });
      if Lobby.Room.is_ready t.room then (
        setup_round t (Game.create ~config:t.game_config ~room:t.room)
      );
      Ok ()
    ) else (
      Error `You're_not_playing
    )
  )

let cancel_all_for_player t ~username ~round =
  match Game.cancel_orders round ~sender:username with
  | Error `You're_not_playing as e -> e
  | Ok orders ->
    List.iter orders ~f:(fun order ->
        Updates_manager.broadcast t.room_updates
          (Broadcast (Out order)));
    Ok `Ack

let player_disconnected t ~username =
  begin match t.game with
  | Some round ->
    begin match cancel_all_for_player t ~username ~round with
    | Error `You're_not_playing | Ok `Ack -> ()
    end
  | None ->
    begin match player_ready t ~username ~is_ready:false with
    | Error (`Game_already_started | `You're_not_playing)
    | Ok () -> ()
    end
  end;
  apply_room_update t (Player_event { username; event = Disconnected })

module Rpc_state = struct
  type nonrec t = { room : t; username : Username.t }
end

let rpc_implementations =
  let implement rpc f =
    Rpc.Rpc.implement rpc (fun r q ->
        match r with
        | Error (`Not_logged_in | `Not_in_a_room as e) ->
          return (Error e)
        | Ok { Rpc_state.room; username } -> f ~room ~username q
      )
  in
  let during_game rpc f =
    implement rpc (fun ~room ~username query ->
        match room.game with
        | None -> return (Error `Game_not_in_progress)
        | Some round -> f ~username ~room ~round query
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
          Time_ns.diff (Game.end_time round) (Time_ns.now ())
        in
        return (Ok span)
      )
  ; during_game Protocol.Get_update.rpc
      (fun ~username ~room ~round which ->
        match which with
        | Hand ->
          begin match Game.get_hand round ~username with
          | Error `You're_not_playing as e -> return e
          | Ok hand ->
              Updates_manager.update room.room_updates ~username
                (Hand hand);
              return (Ok ())
          end
        | Market ->
          Updates_manager.update room.room_updates ~username
            (Market (Game.market round));
          return (Ok ())
      )
  ; during_game Protocol.Order.rpc
      (fun ~username ~room ~round order ->
        match Game.add_order round ~order ~sender:username with
        | (Error _) as e -> return e
        | Ok exec ->
          apply_room_update room (Exec exec);
          return (Ok `Ack)
      )
  ; during_game Protocol.Cancel.rpc
      (fun ~username ~room ~round id ->
        match Game.cancel_order round ~id ~sender:username with
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
