open Core.Std
open Async.Std
module Rpc_kernel = Async_rpc_kernel.Std

module Updates_manager = struct
  (* One username may have many of these clients *)
  module Client = struct
    type 'update t = {
      conn     : Rpc.Connection.t;
      username : Username.t;
      updates  : 'update Pipe.Writer.t;
    }
  end

  type 'update t =
    { clients : 'update Client.t Doubly_linked.t Username.Table.t }

  let create () = { clients = Username.Table.create () }

  let subscribe t (conn : _ Client.t) =
    Hashtbl.update t.clients conn.username
      ~f:(fun conns ->
        let conns =
          match conns with
          | None -> Doubly_linked.create ()
          | Some conns -> conns
        in
        let elt = Doubly_linked.insert_first conns conn in
        don't_wait_for begin
          Pipe.closed conn.updates
          >>| fun () ->
          Doubly_linked.remove conns elt
        end;
        conns
    )

  let has_player t ~username = Hashtbl.mem t.clients username

  let write_update_to_logins (logins : _ Client.t Doubly_linked.t) update =
    Doubly_linked.iter logins ~f:(fun login ->
      Pipe.write_without_pushback login.updates update
    )

  let update t ~username update =
    Option.iter (Hashtbl.find t.clients username) ~f:(fun logins ->
      write_update_to_logins logins update
    )

  let broadcast t broadcast =
    Hashtbl.iteri t.clients ~f:(fun ~key:_ ~data:logins ->
      write_update_to_logins logins broadcast
    )

  let broadcasts t = List.iter ~f:(broadcast t)
end

module Room_manager = struct
  type t =
    { id : Lobby.Room.Id.t
    ; room : Lobby.Room.t
    ; game : Game.t
    ; updates : Protocol.Game_update.t Updates_manager.t
    }

  let create ~game_config ~id =
    { id
    ; room = Lobby.Room.empty
    ; game = Game.create ~config:game_config
    ; updates = Updates_manager.create ()
    }

  let setup_round t (round : Game.Round.t) =
    don't_wait_for begin
      Clock_ns.at round.end_time
      >>| fun () ->
      let results = Game.end_round t.game round in
      Updates_manager.broadcasts t.updates
        [ Broadcast (Round_over results)
        ; Broadcast (Scores (Game.scores t.game))
        ]
    end;
    Map.iteri round.players ~f:(fun ~key:username ~data:p ->
      Updates_manager.update t.updates ~username (Hand p.hand)
    );
    Updates_manager.broadcasts t.updates
      [ Broadcast New_round
      ; Broadcast (Scores (Game.scores t.game))
      ];
end

module Connection_state = struct
  module Status = struct
    type t =
      | Not_logged_in of { conn : Rpc.Connection.t }
      | Logged_in of { conn : Rpc.Connection.t; username : Username.t }
      | Lobby of Protocol.Lobby_update.t Updates_manager.Client.t
      | In_room of
        { player : Protocol.Game_update.t Updates_manager.Client.t
        ; room   : Room_manager.t
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
  ; game_config : Game.Config.t
  }

let lobby_snapshot t =
  Hashtbl.fold t.rooms ~init:Lobby.Room.Id.Map.empty
    ~f:(fun ~key ~data acc -> Map.add acc ~key ~data:data.room)

let new_room_exn t ~id =
  let new_room = Room_manager.create ~id ~game_config:t.game_config in
  Hashtbl.add_exn t.rooms ~key:id ~data:new_room;
  Updates_manager.broadcast t.lobby_updates
    (New_room { id; room = new_room.room })

let unused_room_id t =
  let rec try_ i =
    let id = Lobby.Room.Id.of_string (Int.to_string i) in
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

let create ~game_config =
  let t = 
    { lobby_updates = Updates_manager.create ()
    ; rooms = Lobby.Room.Id.Table.create ()
    ; game_config
    }
  in
  ensure_empty_room_exists t;
  t

let implementations t =
  let username_conn (state : Connection_state.t) =
    match !state with
    | Not_logged_in _ -> Error `Not_logged_in
    | Logged_in { username; conn }
    | Lobby { username; conn; _ }
    | In_room { player = { username; conn; _ }; room = _ } ->
      Ok (username, conn)
  in
  let in_room rpc f =
    Rpc.Rpc.implement rpc
      (fun (state : Connection_state.t) query ->
        match !state with
        | Not_logged_in _ -> return (Error `Not_logged_in)
        | Logged_in _ | Lobby _ -> return (Error `Not_in_a_room)
        | In_room { player; room } ->
          f ~username:player.username ~room query
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
            state := Logged_in { conn; username };
            return (Ok ())
          | _other ->
            return (Error `Already_logged_in))
    ; Rpc.Pipe_rpc.implement Protocol.Get_lobby_updates.rpc
        (fun (state : Connection_state.t) () ->
          (* no reason in principle why we couldn't allow not-logged-in
             users to get lobby updates, it just means writing code *)
          return (username_conn state)
          >>=? fun (username, conn) ->
          let (updates_r, updates_w) = Pipe.create () in
          let client : _ Updates_manager.Client.t =
            { conn; username; updates = updates_w }
          in
          state := Lobby client;
          Updates_manager.subscribe t.lobby_updates client;
          Pipe.write_without_pushback updates_w
            (Snapshot (lobby_snapshot t));
          return (Ok updates_r))
    ; Rpc.Pipe_rpc.implement Protocol.Join_room.rpc
        (fun (state : Connection_state.t) room_id ->
          return begin match !state with
          | Not_logged_in _ -> Error `Not_logged_in
          | In_room _ -> Error `Already_in_a_room
          | Logged_in { username; conn }
          | Lobby { username; conn; _ } -> Ok (username, conn)
          end
          >>=? fun (username, conn) ->
          begin match Hashtbl.find t.rooms room_id with
          | None -> return (Error `No_such_room)
          | Some room -> return (Ok room)
          end
          >>=? fun room ->
          begin if Updates_manager.has_player room.updates ~username then (
            return (Ok ())
          ) else (
            return (Game.player_join room.game ~username)
            >>|? fun () ->
            ensure_empty_room_exists t;
            Updates_manager.broadcast room.updates
              (Broadcast (Player_joined username))
          ) end
          >>=? fun () ->
          let updates_r, updates_w = Pipe.create () in
          let player : _ Updates_manager.Client.t =
            { username; conn; updates = updates_w }
          in
          Updates_manager.subscribe room.updates player;
          state := In_room { player; room };
          let catch_up =
            let open Protocol.Game_update in
            [ [Broadcast (Scores (Game.scores room.game))]
            ; match room.game.phase with
              | Waiting_for_players waiting ->
                List.map (Hashtbl.data waiting.players) ~f:(fun wp ->
                  Broadcast (Player_ready
                    { who = wp.p.username
                    ; is_ready = wp.is_ready
                    }))
              | Playing round ->
                  [ Broadcast New_round
                  ; Hand (Map.find_exn round.players username).hand
                  ; Market round.market
                  ]
            ] |> List.concat
          in
          List.iter catch_up ~f:(fun update ->
            Pipe.write_without_pushback updates_w update);
          return (Ok updates_r)
      )
    ; Rpc.Rpc.implement Protocol.Chat.rpc
        (fun (state : Connection_state.t) msg ->
          match !state with
          | Not_logged_in _ -> return (Error `Not_logged_in)
          | Logged_in { username; _ } | Lobby { username; _ } ->
            Updates_manager.broadcast t.lobby_updates
              (Chat (username, msg));
            return (Ok ())
          | In_room { player; room } ->
            Updates_manager.broadcast room.updates
              (Broadcast (Chat (player.username, msg)));
            return (Ok ())
      )
    ; in_room Protocol.Is_ready.rpc
        (fun ~username ~room is_ready ->
          Updates_manager.broadcast room.updates
            (Broadcast (Player_ready { who = username; is_ready }));
          return (Game.set_ready room.game ~username ~is_ready)
          >>|? function
          | `Started round -> Room_manager.setup_round room round
          | `Still_waiting _wait -> ()
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
              Updates_manager.update room.updates ~username (Hand hand))
          | Market ->
              Updates_manager.update room.updates ~username
                (Market round.market)
          end;
          return (Ok ())
      )
    ; during_game Protocol.Order.rpc
        (fun ~username ~room ~round order ->
          match Game.Round.add_order round ~order ~sender:username with
          | (Error _) as e -> return e
          | Ok exec ->
            Updates_manager.broadcasts room.updates
              [ Broadcast (Exec (order, exec))
              ; Broadcast (Scores (Game.scores room.game))
              ];
            return (Ok `Ack)
      )
    ; during_game Protocol.Cancel.rpc
        (fun ~username ~room ~round id ->
          match Game.Round.cancel_order round ~id ~sender:username with
          | (Error _) as e -> return e
          | Ok order ->
            Updates_manager.broadcast room.updates (Broadcast (Out order));
            return (Ok `Ack)
      )
    ; during_game Protocol.Cancel_all.rpc
        (fun ~username ~room ~round () ->
          match Game.Round.cancel_orders round ~sender:username with
          | (Error _) as e -> return e
          | Ok orders ->
            List.iter orders ~f:(fun order ->
              Updates_manager.broadcast room.updates
                (Broadcast (Out order)));
            return (Ok `Ack)
      )
    ]

let main ~tcp_port ~web_port ~game_config =
  let t = create ~game_config in
  let implementations = implementations t in
  let%bind _server =
    Rpc.Connection.serve
      ~initial_connection_state:(fun addr conn ->
        Deferred.upon (Rpc.Connection.close_reason conn ~on_close:`started)
          (fun reason ->
            Log.Global.sexp ~level:`Info [%message
              "disconnected"
                (addr : Socket.Address.Inet.t)
                (reason : Info.t)
            ]
          );
        Connection_state.create ~conn)
      ~implementations
      ~where_to_listen:(Tcp.on_port tcp_port)
      ()
  and () =
    Web_transport.serve ~port:web_port
      ~f:(fun addr transport ->
        Rpc_kernel.Rpc.Connection.server_with_close transport
          ~implementations
          ~on_handshake_error:`Raise
          ~connection_state:(fun conn ->
              Log.Global.sexp ~level:`Info [%message
                "Web client connected"
                  (addr : Socket.Address.Inet.t)
              ];
              Connection_state.create ~conn
            )
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
      in
      fun () ->
        Log.Global.set_level log_level;
        Random.self_init ();
        main ~tcp_port ~web_port ~game_config
        >>= never_returns
    ]
