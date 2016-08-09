open Core.Std
open Async.Std

module Address = struct
  include Socket.Address.Inet
  include Hashable.Make(struct
      include Socket.Address.Inet
      let compare t1 t2 = String.compare (to_string t1) (to_string t2)
      let hash t = String.hash (to_string t)
    end)
end

module User = struct
  type t = {
    username : Username.t;
    updates : Protocol.Player_update.t Pipe.Writer.t;
  }
end

let main ~game_port ~web_port =
  Web_server.create ~port:web_port
  >>= fun web_server ->
  let game = Game.create () in
  let users : User.t Address.Table.t = Address.Table.create () in
  let users_of_player : User.t list Username.Table.t =
    Username.Table.create ()
  in
  let drop ~addr ~conn ~reason =
    Option.iter (Hashtbl.find_and_remove users addr)
      ~f:(fun user ->
        Hashtbl.change users_of_player user.username
          ~f:(Option.map ~f:(List.filter ~f:(fun u -> not (phys_equal user u))));
        Pipe.close user.updates);
    Rpc.Connection.close ~reason:(Info.t_of_sexp reason) conn
    |> don't_wait_for
  in
  let drop_unknown ~addr ~conn =
    Log.Global.sexp ~level:`Info
      [%message "don't know who this is" (addr : Address.t)];
    drop ~addr ~conn ~reason:[%message "don't know who you are"]
  in
  let broadcast b =
    let update = Protocol.Player_update.Broadcast b in
    Log.Global.sexp [%sexp (update : Protocol.Player_update.t)];
    Web_server.broadcast web_server b;
    Hashtbl.iteri users ~f:(fun ~key:_ ~data:user ->
      Pipe.write_without_pushback user.updates update)
  in
  let broadcast_waiting () =
    broadcast (Waiting_for (Game.waiting_for game))
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
      List.iter (Hashtbl.find_exn users_of_player username) ~f:(fun user ->
        Pipe.write_without_pushback user.updates (Dealt p.hand)))
  in
  let implementations =
    let for_existing_user rpc f =
      Rpc.Rpc.implement rpc
        (fun (addr, conn) query ->
          match Hashtbl.find users addr with
          | None ->
            drop_unknown ~addr ~conn;
            return (Error `You're_not_playing)
          | Some user ->
            f ~user query)
    in
    Rpc.Implementations.create_exn
      ~on_unknown_rpc:`Close_connection
      ~implementations:
      [ Rpc.Pipe_rpc.implement Protocol.Join_game.rpc
          (fun (addr, conn) username ->
            return (Game.player_join game ~username)
            >>|? fun () ->
            let r, w = Pipe.create () in
            Deferred.upon (Pipe.closed r)
              (fun () ->
                drop ~addr ~conn
                  ~reason:[%message "stopped listening to updates"]);
            let user : User.t = { username; updates = w } in
            Hashtbl.set users ~key:addr ~data:user;
            Hashtbl.add_multi users_of_player ~key:username ~data:user;
            broadcast (Player_joined username);
            (* catch this person up on how many we still need *)
            Pipe.write_without_pushback w
              (Broadcast (Waiting_for (Game.waiting_for game)));
            r)
      ; Rpc.Rpc.implement Protocol.Is_ready.rpc
          (fun (addr, conn) is_ready ->
            match Hashtbl.find users addr with
            | None ->
              drop_unknown ~addr ~conn;
              return (Ok ())
            | Some user ->
              match Game.set_ready game ~username:user.username ~is_ready with
              | Error _ as e -> return e
              | Ok (`Started round) -> setup_round round; return (Ok ())
              | Ok `Still_waiting -> broadcast_waiting (); return (Ok ()))
      ; Rpc.One_way.implement Protocol.Chat.rpc
          (fun (addr, conn) msg ->
            match Hashtbl.find users addr with
            | None -> drop_unknown ~addr ~conn
            | Some user ->
              broadcast (Chat (user.username, msg)))
      ; Rpc.Rpc.implement Protocol.Book.rpc
          (fun _ () ->
            match game.phase with
            | Waiting_for_players _ -> return Market.Book.empty
            | Playing round -> return round.market)
      ; for_existing_user Protocol.Hand.rpc
          (fun ~user () ->
            return (Game.get_hand game ~username:user.username))
      ; for_existing_user Protocol.Order.rpc
          (fun ~user order ->
            let r = Game.add_order game ~order ~sender:user.username in
            Result.iter r ~f:(fun exec ->
              broadcast (Exec (order, exec)));
            return r)
      ; for_existing_user Protocol.Cancel.rpc
          (fun ~user id ->
            let r = Game.cancel game ~id ~sender:user.username in
            Result.iter r ~f:(fun order ->
              broadcast (Out order));
            return (Result.ignore r))
      ]
  in
  Rpc.Connection.serve
    ~initial_connection_state:(fun addr conn ->
      Deferred.upon (Rpc.Connection.close_reason conn ~on_close:`started)
        (fun reason ->
          drop ~addr ~conn
            ~reason:[%message "Rpc connection closed" (reason : Info.t)]);
      (addr, conn))
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
      in
      fun () ->
        main ~game_port ~web_port
        >>= never_returns
    ]
