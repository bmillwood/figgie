open Core
open Async
module Rpc_kernel = Async_rpc_kernel

module WS = Websocket_async

let serve ~port ~f =
  let monitor = Monitor.create () in
  Monitor.detach_and_iter_errors monitor ~f:(fun exn ->
    Log.Global.sexp ~level:`Error [%message
      "Error in websocket server"
        ~where:"toplevel"
        (exn : exn)
    ]);
  within' ~monitor (fun () ->
    Tcp.Server.create (Tcp.on_port port)
      (fun addr reader writer ->
        let on_exn ~where exn =
          Log.Global.sexp ~level:`Error [%message
            "Error in websocket server"
              (where : string)
              (addr : Socket.Address.Inet.t)
              (exn : exn)
          ]
        in
        Monitor.detach_and_iter_errors (Writer.monitor writer)
          ~f:(on_exn ~where:"Writer");
        let app_to_ws, to_client = Pipe.create () in
        let from_client, ws_to_app = Pipe.create () in
        let ws_log =
          Log.create
            ~level:`Debug
            ~output:[Log.Output.stdout ()]
            ~on_error:`Raise
        in
        let finished =
          Monitor.try_with (fun () ->
            WS.server
              ~log:ws_log
              ~app_to_ws
              ~ws_to_app
              ~reader
              ~writer
              ()
          )
          >>| function
          | Error exn -> on_exn ~where:"WS.server raised" exn
          | Ok (Error error) -> on_exn ~where:"WS.server error" (Error.to_exn error)
          | Ok (Ok ()) -> ()
        in
        let strings_from_client =
          Pipe.filter_map from_client ~f:(fun frame ->
            match frame.opcode with
            | Close ->
              Pipe.write_without_pushback to_client (WS.Frame.close 1000);
              Pipe.close to_client;
              None
            | Binary -> Some frame.content
            | _ -> None)
        in
        let strings_to_client =
          Pipe.create_writer (fun reader ->
            Pipe.transfer reader to_client ~f:(fun string ->
              WS.Frame.of_bytes ~opcode:Binary string
            )
          )
        in
        let f_result =
          let transport =
            Rpc_kernel.Pipe_transport.(create Kind.string)
              strings_from_client strings_to_client
          in
          f addr transport
        in
        Deferred.any_unit [finished; f_result]
      )
    >>| fun _server ->
    Ok ())
