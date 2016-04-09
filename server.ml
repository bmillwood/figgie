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
    player : Game.Player.t;
    updates : Protocol.Update.t Pipe.Writer.t;
  }
end

let main ~port =
  let game = Game.create () in
  let users : User.t Address.Table.t = Address.Table.create () in
  let users_of_player : User.t list Protocol.Username.Table.t =
    Protocol.Username.Table.create ()
  in
  let drop ~addr ~conn ~reason =
    Option.iter (Hashtbl.find_and_remove users addr)
      ~f:(fun user ->
        Hashtbl.change users_of_player user.player.username
          ~f:(Option.map ~f:(List.filter ~f:(fun u -> not (phys_equal user u))));
        Pipe.close user.updates);
    Rpc.Connection.close ~reason:(Info.t_of_sexp reason) conn
    |> don't_wait_for
  in
  let start_game () =
    Game.deal game;
    Deferred.List.iter ~how:`Parallel (Hashtbl.data users) ~f:(fun user ->
      Pipe.write user.updates (Dealt user.player.hand))
    |> don't_wait_for
  in
  let implementations =
    Rpc.Implementations.create_exn
      ~on_unknown_rpc:`Close_connection
      ~implementations:
      [ (Rpc.Pipe_rpc.implement Protocol.Join_game.rpc
          (fun (addr, conn) username ->
            match game.stage with
            | Playing -> return (Error `Game_already_started)
            | Waiting_for_players ->
              if Game.num_players game >= Params.max_players              
              then return (Error `Game_is_full)
              else begin
                let player = Game.player game ~username in
                let r, w = Pipe.create () in
                Deferred.upon (Pipe.closed r)
                  (fun () ->
                    drop ~addr ~conn
                      ~reason:[%message "stopped listening to updates"]);
                let user : User.t = { player; updates = w } in
                Hashtbl.set users ~key:addr ~data:user;
                Hashtbl.add_multi users_of_player ~key:username ~data:user;
                Pipe.write_without_pushback w (Waiting_for (Game.waiting_for game));
                return (Ok r)
              end))
      ; Rpc.Rpc.implement Protocol.Is_ready.rpc
          (fun (addr, conn) is_ready ->
            match Hashtbl.find users addr with
            | None ->
              drop ~addr ~conn ~reason:[%message "don't know who you are"];
              (* this doesn't really matter *)
              return (Ok ())
            | Some user ->
              match Game.set_ready game ~player:user.player ~is_ready with
              | Error _ as e -> return e
              | Ok `All_ready -> start_game (); return (Ok ())
              | Ok `Still_waiting -> return (Ok ()))
      ]
  in
  Rpc.Connection.serve
    ~initial_connection_state:(fun addr conn ->
      Deferred.upon (Rpc.Connection.close_reason conn)
        (fun reason -> drop ~addr ~conn
          ~reason:[%message "Rpc connection closed" (reason : Info.t)]);
      (addr, conn))
    ~implementations
    ~where_to_listen:(Tcp.on_port port)
    ()
  >>= fun _server ->
  Deferred.never ()

let command =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"Figgie server"
    [%map_open
      let port =
        flag "-port" (optional_with_default 10203 int) ~doc:"N port to listen on"
      in
      fun () ->
        main ~port
        >>= never_returns
    ]
