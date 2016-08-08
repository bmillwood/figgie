open Core.Std
open Async.Std

module WS = Websocket_async

type t = {
  server : (Socket.Address.Inet.t, int) Tcp.Server.t;
  pipe : Web_protocol.Message.t Pipe.Writer.t;
}

let create ~port =
  let pipe_r, pipe = Pipe.create () in
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
        Log.Global.set_level `Debug;
        let _finished =
          Monitor.try_with (fun () ->
            WS.server
              ~log:(Lazy.force Log.Global.log)
              ~app_to_ws
              ~ws_to_app
              ~reader
              ~writer
              (addr :> Socket.Address.t)
          )
          >>| function
          | Error exn -> on_exn ~where:"WS.server" exn
          | Ok () -> ()
        in
        Clock.every ~stop:(Pipe.closed to_client) (sec 1.) (fun () ->
          Pipe.write_without_pushback_if_open to_client
            (WS.Frame.of_bytes ~opcode:Ping (Time.to_string (Time.now ()))));
        let end_transfer =
          Pipe.transfer pipe_r to_client ~f:(fun m ->
            let s = Sexp.to_string [%sexp (m : Web_protocol.Message.t)] in
            WS.Frame.of_bytes ~opcode:Text s)
        in
        let end_read =
          Pipe.iter_without_pushback from_client ~f:(fun frame ->
            match frame.opcode with
            | Close ->
              Pipe.write_without_pushback to_client (WS.Frame.close 1000);
              Pipe.close to_client
            | _ -> ())
        in
        Deferred.all_unit [end_transfer; end_read]
      )
    >>| fun server ->
    { server; pipe })

let broadcast t s = Pipe.write_without_pushback t.pipe (Broadcast s)
