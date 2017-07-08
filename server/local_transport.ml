open! Core
open Async
open Async_rpc_kernel

type t = (Rpc.Transport.t -> unit Deferred.t) Ivar.t

let create () = Ivar.create ()

let listen t ~f = Ivar.fill t f

let connect t =
  let from_client, to_server = Pipe.create () in
  let from_server, to_client = Pipe.create () in
  let client_transport =
    Pipe_transport.(create Kind.bigstring)
      from_client to_client
  in
  let server_transport =
    Pipe_transport.(create Kind.bigstring)
      from_server to_server
  in
  don't_wait_for (
    let%bind f = Ivar.read t in
    f server_transport
  );
  Rpc.Connection.create
    ~connection_state:(fun _ -> ())
    client_transport
  |> Deferred.Or_error.of_exn_result
