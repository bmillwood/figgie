open Core
open Async

open Figgie

type t =
  | First_available
  | Named of Lobby.Room.Id.t
[@@deriving sexp]

let param =
  let open Command.Param in
  flag "-room"
    (optional_with_default First_available
       (Arg_type.create (fun s -> Named (Lobby.Room.Id.of_string s))))
    ~doc:"ID room to join on startup, defaults to first available"

let join_any_room ~conn ~my_name =
  let%bind lobby_updates =
    match%map
      Rpc.Pipe_rpc.dispatch Protocol.Get_lobby_updates.rpc conn ()
      >>| ok_exn
    with
    | Error `Not_logged_in -> assert false
    | Ok (lobby_updates, _metadata) -> lobby_updates
  in
  let can_join room =
    not (Lobby.Room.is_full room)
    || Lobby.Room.has_player room ~username:my_name
  in
  let try_to_join id =
    match%map
      Rpc.Pipe_rpc.dispatch Protocol.Join_room.rpc conn id
      >>| ok_exn
    with
    | Ok (updates, _pipe_metadata) ->
      Pipe.close_read lobby_updates;
      `Finished (id, updates)
    | Error (`Already_in_a_room | `Not_logged_in) -> assert false
    | Error `No_such_room -> `Repeat ()
  in
  Deferred.repeat_until_finished ()
    (fun () ->
      let%bind update =
        match%map Pipe.read lobby_updates with
        | `Eof -> failwith "Server hung up on us"
        | `Ok update -> update
      in
      match update with
      | Lobby_snapshot lobby ->
        begin match
            List.find (Map.to_alist lobby.rooms) ~f:(fun (_id, room) ->
                can_join room)
          with
          | Some (id, room) ->
            Log.Global.sexp ~level:`Debug [%message
              "doesn't look full, joining"
                (id : Lobby.Room.Id.t) (room : Lobby.Room.t)
            ];
            try_to_join id
          | None ->
            Log.Global.sexp ~level:`Debug [%message
              "couldn't see an empty room, waiting"
            ];
            return (`Repeat ())
        end
      | Lobby_update (New_empty_room { room_id }) ->
        try_to_join room_id
      | Lobby_update (Lobby_update _ | Room_closed _ | Room_update _)
      | Chat _ -> return (`Repeat ())
    )

let join t ~conn ~my_name =
  match t with
  | First_available ->
    join_any_room ~conn ~my_name
  | Named id ->
    let%map (updates, _metadata) =
      Rpc.Pipe_rpc.dispatch_exn Protocol.Join_room.rpc conn id
    in
    (id, updates)
