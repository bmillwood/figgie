open Core.Std
open Async.Std
module Rpc_kernel = Async_rpc_kernel.Std

module Connection_state = struct
  (* One username may have many of these connections *)
  module Player = struct
    type t = {
      conn     : Rpc.Connection.t;
      username : Username.t;
      updates  : Protocol.Player_update.t Pipe.Writer.t;
    }
  end

  module Observer = struct
    type t = {
      conn     : Rpc.Connection.t;
      updates  : Protocol.Observer_update.t Pipe.Writer.t;
    }
  end

  module Status = struct
    type t =
      | Not_logged_in of Rpc.Connection.t
      | Player of Player.t
      | Observer of Observer.t
  end

  type t = Status.t ref

  let create ~conn : t =
    ref (Status.Not_logged_in conn)
end

module Connection_manager = struct
  open Connection_state
  type t = {
    players   : Player.t Doubly_linked.t Username.Table.t;
    observers : Observer.t Doubly_linked.t;
  }

  let create () =
    { players   = Username.Table.create ()
    ; observers = Doubly_linked.create ()
    }

  let add_player_conn t (conn : Player.t) =
    Hashtbl.update t.players conn.username
      ~f:(fun conns ->
        let conns =
          match conns with
          | None -> Doubly_linked.create ()
          | Some conns -> conns
        in
        let elt = Doubly_linked.insert_first conns conn in
        don't_wait_for begin
          Pipe.closed conn.updates
          >>= fun () ->
          Doubly_linked.remove conns elt;
          Rpc.Connection.close conn.conn
        end;
        conns
    )

  let has_player t ~username = Hashtbl.mem t.players username

  let add_observer t (observer : Observer.t) =
    let elt = Doubly_linked.insert_last t.observers observer in
    don't_wait_for begin
      Pipe.closed observer.updates
      >>= fun () ->
      Doubly_linked.remove t.observers elt;
      Rpc.Connection.close observer.conn
    end

  let write_update_to_logins (logins : Player.t Doubly_linked.t) update =
    Doubly_linked.iter logins ~f:(fun login ->
      Pipe.write_without_pushback login.updates update
    )

  let player_update t ~username update =
    Option.iter (Hashtbl.find t.players username) ~f:(fun logins ->
      write_update_to_logins logins update
    )

  let observer_update t update =
    Doubly_linked.iter t.observers ~f:(fun observer ->
      Pipe.write_without_pushback observer.updates update
    )

  let observer_updates t updates = List.iter updates ~f:(observer_update t)

  let broadcast t broadcast =
    Log.Global.sexp [%sexp (broadcast : Protocol.Broadcast.t)];
    Hashtbl.iteri t.players ~f:(fun ~key:_ ~data:logins ->
      write_update_to_logins logins (Broadcast broadcast)
    );
    observer_update t (Broadcast broadcast)

  let broadcasts t updates = List.iter updates ~f:(broadcast t)
end

let implementations () =
  let game = Game.create () in
  let conns = Connection_manager.create () in
  let setup_round (round : Game.Round.t) =
    don't_wait_for begin
      Clock_ns.at round.end_time
      >>| fun () ->
      let results = Game.end_round game round in
      Connection_manager.broadcasts conns
        [ Round_over results
        ; Scores (Game.scores game)
        ]
    end;
    Map.iteri round.players ~f:(fun ~key:username ~data:p ->
      Connection_manager.player_update conns ~username (Hand p.hand)
    );
    Connection_manager.broadcasts conns
      [ New_round
      ; Scores (Game.scores game)
      ];
    Connection_manager.observer_updates conns
      [ Hands (Game.Round.hands round)
      ; Gold round.gold
      ]
  in
  let for_existing_user rpc f =
    Rpc.Rpc.implement rpc
      (fun (state : Connection_state.t) query ->
        match !state with
        | Not_logged_in _ | Observer _ -> return (Error `Login_first)
        | Player { username; _ } -> f ~username query
    )
  in
  let during_game rpc f =
    for_existing_user rpc (fun ~username query ->
      match game.phase with
      | Waiting_for_players _ -> return (Error `Game_not_in_progress)
      | Playing round -> f ~username ~round query
    )
  in
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:
    [ Rpc.Pipe_rpc.implement Protocol.Login.rpc
        (fun (state : Connection_state.t) username ->
          let login ?(run_if_ok=ignore) conn =
            begin if Connection_manager.has_player conns ~username
            then return (Ok ())
            else begin
              return (Game.player_join game ~username)
              >>|? fun () ->
              Connection_manager.broadcast conns (Player_joined username)
            end end
            >>|? fun () ->
            run_if_ok ();
            let updates_r, updates_w = Pipe.create () in
            let player_conn : Connection_state.Player.t =
              { username; conn; updates = updates_w }
            in
            Connection_manager.add_player_conn conns player_conn;
            state := Player player_conn;
            let catch_up =
              let open Protocol.Player_update in
              [ [Broadcast (Scores (Game.scores game))]
              ; match game.phase with
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
            updates_r
          in
          match !state with
          | Player _ -> return (Error `Already_logged_in)
          | Not_logged_in conn ->
            login conn
          | Observer observer ->
            login
              ~run_if_ok:(fun () -> Pipe.close observer.updates)
              observer.conn
        )
    ; Rpc.Pipe_rpc.implement Protocol.Get_observer_updates.rpc
        (fun (state : Connection_state.t) () ->
          match !state with
          | Player _ | Observer _ -> return (Error `Already_logged_in)
          | Not_logged_in conn ->
            let updates_r, updates_w = Pipe.create () in
            let observer : Connection_state.Observer.t =
              { conn; updates = updates_w }
            in
            Connection_manager.add_observer conns observer;
            state := Observer observer;
            let catch_up : Protocol.Observer_update.t list =
              match game.phase with
              | Waiting_for_players _wait ->
                []
              | Playing round ->
                [ Market round.market
                ; Hands (Game.Round.hands round)
                ; Gold round.gold
                ; Broadcast (Scores (Game.scores game))
                ]
            in
            List.iter catch_up ~f:(Pipe.write_without_pushback updates_w);
            return (Ok updates_r)
      )
    ; Rpc.Rpc.implement Protocol.Time_remaining.rpc
        (fun _state () ->
          match game.phase with
          | Waiting_for_players _ -> return (Error `Game_not_in_progress)
          | Playing round ->
            let span = Time_ns.diff round.end_time (Time_ns.now ()) in
            return (Ok span)
      )
    ; for_existing_user Protocol.Is_ready.rpc
        (fun ~username is_ready ->
          Connection_manager.broadcast conns
            (Player_ready { who = username; is_ready });
          return (Game.set_ready game ~username ~is_ready)
          >>|? function
          | `Started round -> setup_round round
          | `Still_waiting _wait -> ()
      )
    ; for_existing_user Protocol.Chat.rpc
        (fun ~username msg ->
          Connection_manager.broadcast conns (Chat (username, msg));
          return (Ok ()))
    ; during_game Protocol.Get_update.rpc
        (fun ~username ~round which ->
          begin match which with
          | Hand ->
            Result.iter (Game.Round.get_hand round ~username) ~f:(fun hand ->
              Connection_manager.player_update conns ~username
                (Hand hand))
          | Market ->
            Connection_manager.player_update conns ~username
              (Market round.market)
          end;
          return (Ok ()))
    ; during_game Protocol.Order.rpc
        (fun ~username ~round order ->
          match Game.Round.add_order round ~order ~sender:username with
          | (Error _) as e -> return e
          | Ok exec ->
            Connection_manager.broadcasts conns
              [ Exec (order, exec)
              ; Scores (Game.scores game)
              ];
            Connection_manager.observer_updates conns
              [ Hands (Game.Round.hands round)
              ; Market round.market
              ];
            return (Ok `Ack))
    ; during_game Protocol.Cancel.rpc
        (fun ~username ~round id ->
          match Game.Round.cancel_order round ~id ~sender:username with
          | (Error _) as e -> return e
          | Ok order ->
            Connection_manager.broadcast conns (Out order);
            Connection_manager.observer_update conns (Market round.market);
            return (Ok `Ack))
    ]

let main ~tcp_port ~web_port =
  let implementations = implementations () in
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
      in
      fun () ->
        Log.Global.set_level log_level;
        main ~tcp_port ~web_port
        >>= never_returns
    ]
