open Core.Std
open Async.Std

module WS = Websocket_async

type t = {
  pipe : Protocol.Web_update.t Pipe.Writer.t;
}

let create ~port =
  let broadcast_recipients = String.Table.create () in
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
        Hashtbl.set broadcast_recipients
          ~key:(Socket.Address.Inet.to_string addr)
          ~data:to_client;
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
              (addr :> Socket.Address.t)
          )
          >>| function
          | Error exn -> on_exn ~where:"WS.server" exn
          | Ok () -> ()
        in
        Clock.every ~stop:(Pipe.closed to_client) (sec 1.) (fun () ->
          Pipe.write_without_pushback_if_open to_client
            (WS.Frame.of_bytes ~opcode:Ping (Time.to_string (Time.now ()))));
        let end_read =
          Pipe.iter_without_pushback from_client ~f:(fun frame ->
            match frame.opcode with
            | Close ->
              Pipe.write_without_pushback to_client (WS.Frame.close 1000);
              Pipe.close to_client
            | _ -> ())
        in
        let end_write = Pipe.closed to_client in
        Deferred.any_unit [finished; end_read; end_write]
      )
    >>| fun _server ->
    let pipe =
      Pipe.create_writer (fun reader ->
        Pipe.iter_without_pushback reader ~f:(fun update ->
          Hashtbl.filteri_inplace broadcast_recipients
            ~f:(fun ~key:_ ~data:pipe ->
              if Pipe.is_closed pipe
              then false
              else begin
                Pipe.write_without_pushback pipe
                  ([%sexp (update : Protocol.Web_update.t)]
                  |> Sexp.to_string
                  |> WS.Frame.of_bytes ~opcode:Text);
                true
              end)))
    in
    { pipe })

let broadcast t b = Pipe.write_without_pushback t.pipe (Broadcast b)
let market    t m = Pipe.write_without_pushback t.pipe (Market m)
