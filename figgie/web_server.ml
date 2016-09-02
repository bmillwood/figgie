open Core.Std
open Async.Std
module Rpc_kernel = Async_rpc_kernel.Std

module WS = Websocket_async

type t = {
  pipe : Protocol.Web_update.t Pipe.Writer.t;
}

let create ~port =
  let update_recipients = Queue.create () in
  let implementations =
    Rpc.Implementations.create_exn
      ~on_unknown_rpc:`Raise
      ~implementations:
        [ Rpc.Pipe_rpc.implement Protocol.Get_web_updates.rpc
            (fun () () ->
              let r, w = Pipe.create () in
              Queue.enqueue update_recipients w;
              return (Ok r))
        ]
  in
  let end_ =
    Web_transport.serve ~port
      ~f:(fun _addr transport ->
        Rpc_kernel.Rpc.Connection.server_with_close transport
          ~implementations
          ~on_handshake_error:`Raise
          ~connection_state:(fun _ -> ()))
    >>| function
    | Ok () | Error () -> ()
  in
  don't_wait_for end_;
  let pipe =
    Pipe.create_writer (fun reader ->
      Pipe.iter_without_pushback reader ~f:(fun update ->
        Queue.filter_inplace update_recipients ~f:(fun w ->
          if Pipe.is_closed w
          then false
          else begin
            Pipe.write_without_pushback w update;
            true
          end)))
  in
  return { pipe }
      

let broadcast t b = Pipe.write_without_pushback t.pipe (Broadcast b)
let market    t m = Pipe.write_without_pushback t.pipe (Market m)
let hands     t h = Pipe.write_without_pushback t.pipe (Hands h)
