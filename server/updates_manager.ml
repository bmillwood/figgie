open Core
open Async

open Figgie

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
