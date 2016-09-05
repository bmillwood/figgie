open Core.Std
open Async.Std

module Connection_state = struct
  module Connection_info = struct
    type t = {
      conn    : Rpc.Connection.t;
    }
  end

  module Login_info = struct
    type t = {
      conn     : Connection_info.t;
      username : Username.t;
      updates  : Protocol.Player_update.t Pipe.Writer.t;
    }
  end

  module Login_status = struct
    type t =
      | Not_logged_in of Connection_info.t
      | Logged_in of Login_info.t
  end

  type t = Login_status.t ref

  let create ~conn : t =
    ref (Login_status.Not_logged_in { conn })
end

module Connection_manager = struct
  open Connection_state
  type t = {
    conns : Login_info.t Doubly_linked.t Username.Table.t;
  }

  let create () = { conns = Username.Table.create () }

  let login t (login_info : Login_info.t) =
    Hashtbl.update t.conns login_info.username
      ~f:(fun logins ->
        let logins =
          match logins with
          | None -> Doubly_linked.create ()
          | Some logins -> logins
        in
        let elt = Doubly_linked.insert_first logins login_info in
        don't_wait_for begin
          Pipe.closed login_info.updates
          >>= fun () ->
          Doubly_linked.remove logins elt;
          Rpc.Connection.close
            ~reason:(Info.of_string "stopped listening to updates")
            login_info.conn.conn
        end;
        logins)

  let write_update_to_logins (logins : Login_info.t Doubly_linked.t) update =
    Doubly_linked.iter logins ~f:(fun login ->
      Pipe.write_without_pushback login.updates update
    )

  let update t ~username ~update =
    Option.iter (Hashtbl.find t.conns username) ~f:(fun logins ->
      write_update_to_logins logins update
    )

  let broadcast t broadcast =
    Hashtbl.iteri t.conns ~f:(fun ~key:_ ~data:logins ->
      write_update_to_logins logins (Broadcast broadcast)
    )
end

let main ~game_port ~web_port =
  Web_server.create ~port:web_port
  >>= fun web_server ->
  let game = Game.create () in
  let conns = Connection_manager.create () in
  let broadcast broadcast =
    Log.Global.sexp [%sexp (broadcast : Protocol.Broadcast.t)];
    Web_server.broadcast web_server broadcast;
    Connection_manager.broadcast conns broadcast
  in
  let broadcast_waiting () =
    broadcast (Waiting_for (Game.waiting_for game))
  in
  let update_web_market () =
    match game.phase with
    | Waiting_for_players _ -> ()
    | Playing round -> Web_server.market web_server round.market
  in
  let update_web_hands () =
    match game.phase with
    | Waiting_for_players _ -> ()
    | Playing round ->
      let hands = Map.map round.players ~f:(fun p -> p.hand) in
      Web_server.hands web_server hands
  in
  let setup_round (round : Game.Round.t) =
    don't_wait_for begin
      Clock.after Params.length_of_round
      >>| fun () ->
      let results = Game.end_round game round in
      broadcast (Round_over results);
      broadcast_waiting ()
    end;
    Map.iteri round.players ~f:(fun ~key:username ~data:p ->
      Connection_manager.update conns ~username ~update:(Dealt p.hand)
    );
    update_web_hands ()
  in
  let implementations =
    let for_existing_user rpc f =
      Rpc.Rpc.implement rpc
        (fun (state : Connection_state.t) query ->
          match !state with
          | Not_logged_in _ -> return (Error `Login_first)
          | Logged_in { username; _ } ->
            f ~username query)
    in
    Rpc.Implementations.create_exn
      ~on_unknown_rpc:`Close_connection
      ~implementations:
      [ Rpc.Pipe_rpc.implement Protocol.Login.rpc
          (fun (state : Connection_state.t) username ->
            match !state with
            | Logged_in _ -> return (Error `Already_logged_in)
            | Not_logged_in conn ->
              return (Game.player_join game ~username)
              >>|? fun () ->
              let r, w = Pipe.create () in
              let login_info : Connection_state.Login_info.t =
                { username; conn; updates = w }
              in
              Connection_manager.login conns login_info;
              state := Logged_in login_info;
              broadcast (Player_joined username);
              (* catch this person up on how many we still need *)
              Pipe.write_without_pushback w
                (Broadcast (Waiting_for (Game.waiting_for game)));
              r)
      ; for_existing_user Protocol.Is_ready.rpc
          (fun ~username is_ready ->
              match Game.set_ready game ~username ~is_ready with
              | Error _ as e -> return e
              | Ok (`Started round) -> setup_round round; return (Ok ())
              | Ok `Still_waiting -> broadcast_waiting (); return (Ok ()))
      ; for_existing_user Protocol.Chat.rpc
          (fun ~username msg ->
            broadcast (Chat (username, msg));
            return (Ok ()))
      ; Rpc.Rpc.implement Protocol.Book.rpc
          (fun _ () ->
            match game.phase with
            | Waiting_for_players _ -> return Market.Book.empty
            | Playing round -> return round.market)
      ; for_existing_user Protocol.Hand.rpc
          (fun ~username () ->
            return (Game.get_hand game ~username))
      ; for_existing_user Protocol.Order.rpc
          (fun ~username order ->
            let r = Game.add_order game ~order ~sender:username in
            Result.iter r ~f:(fun exec ->
              broadcast (Exec (order, exec));
              update_web_hands ();
              update_web_market ());
            return r)
      ; for_existing_user Protocol.Cancel.rpc
          (fun ~username id ->
            let r = Game.cancel game ~id ~sender:username in
            Result.iter r ~f:(fun order ->
              broadcast (Out order);
              update_web_market ());
            return (Result.ignore r))
      ]
  in
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
    ~where_to_listen:(Tcp.on_port game_port)
    ()
  >>= fun _server ->
  Deferred.never ()

let command =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"Figgie server"
    [%map_open
      let game_port =
        flag "-game-port" (optional_with_default 10203 int)
          ~doc:"N port to listen on for players"
      and web_port =
        flag "-web-port" (optional_with_default 20406 int)
          ~doc:"N port to listen on for web UI"
      and log_level =
        flag "-log-level" (optional_with_default `Info Log.Level.arg)
          ~doc:"LEVEL Error, Info, or Debug"
      in
      fun () ->
        Log.Global.set_level log_level;
        main ~game_port ~web_port
        >>= never_returns
    ]
