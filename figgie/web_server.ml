open Core.Std
open Async.Std

module WS = Websocket_async

type t = {
  server : (Socket.Address.Inet.t, int) Tcp.Server.t;
  broadcast : string Pipe.Writer.t;
}

let create ~port =
  let broadcast_r, broadcast = Pipe.create () in
  Tcp.Server.create (Tcp.on_port port)
    (fun addr reader writer ->
      let app_to_ws, to_client = Pipe.create () in
      let from_client, ws_to_app = Pipe.create () in
      Log.Global.set_level `Debug;
      let _finished =
        WS.server
          ~log:(Lazy.force Log.Global.log)
          ~app_to_ws
          ~ws_to_app
          ~reader
          ~writer
          (addr :> Socket.Address.t)
      in
      let end_transfer =
        Pipe.transfer broadcast_r to_client ~f:(fun s ->
          WS.Frame.of_bytes (String.copy s))
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
  { server; broadcast }

let broadcast t s = Pipe.write_without_pushback t.broadcast s
