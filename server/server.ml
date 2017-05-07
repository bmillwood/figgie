open Core
open Async
module Rpc_kernel = Async_rpc_kernel

open Figgie

module Updates_manager = struct
  module Client = struct
    type 'update t = { updates : 'update Pipe.Writer.t }

    let create ~updates = { updates }

    let closed t = Pipe.closed t.updates

    let is_closed t = Pipe.is_closed t.updates

    let ghost t = Pipe.close t.updates

    let update t update =
      Pipe.write_without_pushback_if_open t.updates update
  end

  type 'update t =
    { clients : 'update Client.t Username.Table.t }

  let create () = { clients = Username.Table.create () }

  let subscribe t ~username ~updates =
    Hashtbl.update t.clients username
      ~f:(fun old_client ->
        Option.iter old_client ~f:Client.ghost;
        let new_client = Client.create ~updates in
        don't_wait_for begin
          Client.closed new_client
          >>| fun () ->
          Hashtbl.change t.clients username ~f:(Option.filter ~f:(fun c ->
            not (Client.is_closed c)))
        end;
        new_client
    )

  let update t ~username update =
    Option.iter (Hashtbl.find t.clients username) ~f:(fun client ->
      Client.update client update
    )

  let broadcast t broadcast =
    Hashtbl.iteri t.clients ~f:(fun ~key:_ ~data:client ->
      Client.update client broadcast
    )
end

module Room_manager = struct
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
        Map.mem (Lobby.Room.seating t.room) seat
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
        | Waiting_for_players waiting ->
          List.map (Hashtbl.data waiting.players) ~f:(fun wp ->
            Broadcast (Player_ready
              { who = wp.p.username
              ; is_ready = wp.is_ready
              }))
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

  let setup_round t (round : Game.Round.t) =
    don't_wait_for begin
      Clock_ns.at round.end_time
      >>| fun () ->
      let results = Game.end_round t.game round in
      Updates_manager.broadcast t.room_updates
        (Broadcast (Round_over results));
      broadcast_scores t
    end;
    Map.iteri round.players ~f:(fun ~key:username ~data:p ->
      Updates_manager.update t.room_updates ~username (Hand p.hand)
    );
    Updates_manager.broadcast t.room_updates
      (Broadcast New_round);
    broadcast_scores t

  let player_ready t ~username ~is_ready =
    Updates_manager.broadcast t.room_updates
      (Broadcast (Player_ready { who = username; is_ready }));
    Result.map (Game.set_ready t.game ~username ~is_ready)
      ~f:(function
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
end

module Connection_state = struct
  module Status = struct
    type t =
      | Not_logged_in of { conn : Rpc.Connection.t }
      | Logged_in of
        { conn : Rpc.Connection.t
        ; username : Username.t
        ; room : Room_manager.t option
        }
  end
  open Status

  type t = Status.t ref

  let create ~conn =
    ref (Not_logged_in { conn })
end

type t =
  { lobby_updates : Protocol.Lobby_update.t Updates_manager.t
  ; rooms : Room_manager.t Lobby.Room.Id.Table.t
  ; others : int Username.Map.t
  ; game_config : Game.Config.t
  ; chat_enabled : bool
  }

let lobby_snapshot t : Lobby.t =
  { rooms =
      Hashtbl.fold t.rooms ~init:Lobby.Room.Id.Map.empty
        ~f:(fun ~key ~data acc -> Map.add acc ~key ~data:data.room)
  ; others = t.others
  }

let new_room_exn t ~id =
  let new_room =
    Room_manager.create ~id ~game_config:t.game_config
      ~lobby_updates:t.lobby_updates
  in
  Hashtbl.add_exn t.rooms ~key:id ~data:new_room;
  Updates_manager.broadcast t.lobby_updates
    (Lobby_update (New_empty_room { room_id = id }))

let unused_room_id t =
  let rec try_ i =
    let id = Lobby.Room.Id.of_string (sprintf "Room #%d" (i + 1)) in
    if Hashtbl.mem t.rooms id then (
      try_ (i + 1)
    ) else (
      id
    )
  in
  try_ 0

let ensure_empty_room_exists t =
  if Hashtbl.for_all t.rooms ~f:(fun room -> Game.num_players room.game > 0)
  then (
    new_room_exn t ~id:(unused_room_id t)
  )

let create ~game_config ~chat_enabled =
  let t = 
    { lobby_updates = Updates_manager.create ()
    ; rooms = Lobby.Room.Id.Table.create ()
    ; others = Username.Map.empty
    ; game_config
    ; chat_enabled
    }
  in
  ensure_empty_room_exists t;
  t

let player_disconnected t ~(connection_state : Connection_state.t) =
  match !connection_state with
  | Not_logged_in _ -> ()
  | Logged_in { conn = _; username; room } ->
    match room with
    | None ->
      Updates_manager.broadcast t.lobby_updates
        (Lobby_update (Lobby_update { username; event = Disconnected }))
    | Some room ->
      Room_manager.player_disconnected room ~username

let implementations t =
  let in_room rpc f =
    Rpc.Rpc.implement rpc
      (fun (state : Connection_state.t) query ->
        match !state with
        | Not_logged_in _ -> return (Error `Not_logged_in)
        | Logged_in { room = None; _ } -> return (Error `Not_in_a_room)
        | Logged_in { room = Some room; username; conn = _ } ->
          f ~username ~room query
    )
  in
  let during_game rpc f =
    in_room rpc (fun ~username ~room query ->
      match room.game.phase with
      | Waiting_for_players _ -> return (Error `Game_not_in_progress)
      | Playing round -> f ~username ~room ~round query)
  in
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:
    [ Rpc.Rpc.implement Protocol.Login.rpc
        (fun (state : Connection_state.t) username ->
          match !state with
          | Not_logged_in { conn } ->
            if Username.is_valid username then (
              state := Logged_in { conn; username; room = None };
              Updates_manager.broadcast t.lobby_updates
                (Lobby_update (Lobby_update { username; event = Connected }));
              return (Ok ())
            ) else (
              return (Error `Invalid_username)
            )
          | Logged_in _ ->
            return (Error `Already_logged_in))
    ; Rpc.Pipe_rpc.implement Protocol.Get_lobby_updates.rpc
        (fun (state : Connection_state.t) () ->
          (* no reason in principle why we couldn't allow not-logged-in
             users to get lobby updates, it just means writing code *)
          return begin match !state with
          | Not_logged_in _ -> Error `Not_logged_in
          | Logged_in { username; _ } -> Ok username
          end
          >>=? fun username ->
          let (updates_r, updates_w) = Pipe.create () in
          Updates_manager.subscribe t.lobby_updates
            ~username ~updates:updates_w;
          Pipe.write_without_pushback updates_w
            (Lobby_snapshot (lobby_snapshot t));
          return (Ok updates_r))
    ; Rpc.Pipe_rpc.implement Protocol.Join_room.rpc
        (fun (state : Connection_state.t) room_id ->
          return begin match !state with
          | Not_logged_in _ -> Error `Not_logged_in
          | Logged_in { room = Some _; _ } -> Error `Already_in_a_room
          | Logged_in { room = None; username; conn } -> Ok (username, conn)
          end
          >>=? fun (username, conn) ->
          begin match Hashtbl.find t.rooms room_id with
          | None -> return (Error `No_such_room)
          | Some room -> return (Ok room)
          end
          >>=? fun room ->
          let updates_r = Room_manager.player_join room ~username in
          ensure_empty_room_exists t;
          state := Logged_in { username; conn; room = Some room };
          return (Ok updates_r)
      )
    ; Rpc.Rpc.implement Protocol.Start_playing.rpc
        (fun (state : Connection_state.t) in_seat ->
          return begin match !state with
          | Not_logged_in _ -> Error `Not_logged_in
          | Logged_in { room = None; _ } -> Error `Not_in_a_room
          | Logged_in { room = Some room; username; conn = _ } ->
            Ok (room, username)
          end
          >>=? fun (room, username) ->
          return (Room_manager.start_playing room ~username ~in_seat)
        )
    ; Rpc.Rpc.implement Protocol.Delete_room.rpc
        (fun (_state : Connection_state.t) room_id ->
          match Hashtbl.find t.rooms room_id with
          | None -> return (Error `No_such_room)
          | Some rm ->
            if not (Lobby.Room.can_delete rm.room) then (
              return (Error `Room_in_use)
            ) else (
              Hashtbl.remove t.rooms room_id;
              Updates_manager.broadcast t.lobby_updates
                (Lobby_update (Room_closed { room_id }));
              ensure_empty_room_exists t;
              return (Ok ())
            )
      )
    ; Rpc.Rpc.implement Protocol.Chat.rpc
        (fun (state : Connection_state.t) msg ->
          if not t.chat_enabled then (
            return (Error `Chat_disabled)
          ) else (
            match !state with
            | Not_logged_in _ -> return (Error `Not_logged_in)
            | Logged_in { room = None; username; conn = _ } ->
              Updates_manager.broadcast t.lobby_updates
                (Chat (username, msg));
              return (Ok ())
            | Logged_in { room = Some room; username; conn = _ } ->
              Updates_manager.broadcast room.room_updates
                (Broadcast (Chat (username, msg)));
              return (Ok ())
          )
      )
    ; in_room Protocol.Is_ready.rpc
        (fun ~username ~room is_ready ->
          return (Room_manager.player_ready room ~username ~is_ready)
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
            Room_manager.broadcast_scores room;
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
          return (Room_manager.cancel_all_for_player room ~username ~round)
      )
    ]

let main ~tcp_port ~web_port ~game_config ~chat_enabled =
  let t = create ~game_config ~chat_enabled in
  let implementations = implementations t in
  let initial_connection_state addr conn =
    let state = Connection_state.create ~conn in
    Deferred.upon (Rpc.Connection.close_reason conn ~on_close:`started)
      (fun reason ->
        player_disconnected t ~connection_state:state;
        Log.Global.sexp ~level:`Info [%message
          "disconnected"
            (addr : Socket.Address.Inet.t)
            (reason : Info.t)
          ]
      );
    state
  in
  let%bind _server =
    Rpc.Connection.serve
      ~initial_connection_state
      ~implementations
      ~where_to_listen:(Tcp.on_port tcp_port)
      ()
  and () =
    Web_transport.serve ~port:web_port
      ~f:(fun addr transport ->
        Rpc_kernel.Rpc.Connection.server_with_close transport
          ~implementations
          ~on_handshake_error:`Raise
          ~connection_state:(fun conn -> initial_connection_state addr conn)
      )
    >>| function
    | Ok () | Error () -> ()
  in
  Deferred.never ()

let command =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"Figgie server"
    [%map_open
      let tcp_port =
        flag "-tcp-port"
          (optional_with_default Protocol.default_async_rpc_port int)
          ~doc:"N port to listen on for Async-RPC clients"
      and web_port =
        flag "-web-port"
          (optional_with_default Protocol.default_websocket_port int)
          ~doc:"N port to listen on for websocket clients"
      and log_level =
        flag "-log-level" (optional_with_default `Info Log.Level.arg)
          ~doc:"LEVEL Error, Info, or Debug"
      and game_config = Game.Config.arg
      and chat_enabled =
        flag "-enable-chat" (required bool)
          ~doc:"BOOL enable player-player comnunications"
      in
      fun () ->
        Log.Global.set_level log_level;
        Random.self_init ();
        main ~tcp_port ~web_port ~game_config ~chat_enabled
        >>= never_returns
    ]
