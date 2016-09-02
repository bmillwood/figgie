open Core.Std
open Async.Std
module Rpc_kernel = Async_rpc_kernel.Std
module Rpc_transport = Rpc_kernel.Rpc.Transport

module WS = Websocket_async

module WS_reader = struct
  type t = {
    from_client : Bigstring.t Pipe.Reader.t;
    unconsumed  : Bigsubstring.t ref;
  }

  let sexp_of_t _t = Sexp.Atom "<websocket>"

  let close t = Pipe.close_read t.from_client; Deferred.unit
  let is_closed t = Pipe.is_closed t.from_client

  let read_forever t =
    Transport_util.read_forever_of_chunk_pipe
      t.from_client
      ~unconsumed:t.unconsumed
      ~is_closed:(fun () -> is_closed t)
end
let ws_reader =
  (module WS_reader : Rpc_transport.Reader.S with type t = WS_reader.t)

module WS_writer = struct
  type t = {
    to_client : Bigstring.t Pipe.Writer.t;
    monitor   : Monitor.t;
  }

  let sexp_of_t _t = Sexp.Atom "<websocket>"

  let close t = Pipe.close t.to_client; Deferred.unit
  let is_closed t = Pipe.is_closed t.to_client

  let monitor t = t.monitor
  let bytes_to_write t =
    (* ??? I dunno *)
    if Pipe.is_empty t.to_client then 1 else 0
  let stopped t = Pipe.closed t.to_client
  let flushed t =
    Deferred.ignore (Pipe.downstream_flushed t.to_client)
  let ready_to_write = flushed

  let send_bigstring t len write_bigstring =
    if is_closed t
    then Rpc.Transport.Send_result.Closed
    else begin
      let bigstring = Bigstring.create (len + Rpc.Transport.Header.length) in
      Rpc.Transport.Header.unsafe_set_payload_length bigstring ~pos:0 len;
      let _pos = write_bigstring bigstring ~pos:Rpc.Transport.Header.length in
      Pipe.write_without_pushback t.to_client bigstring;
      Rpc.Transport.Send_result.Sent ()
    end

  let send_bin_prot t bin_writer v =
    send_bigstring t (bin_writer.Bin_prot.Type_class.size v)
      (fun bigstring ~pos -> bin_writer.write bigstring ~pos v)
    
  let send_bin_prot_and_bigstring t bin_writer v ~buf ~pos ~len =
    send_bigstring t (bin_writer.Bin_prot.Type_class.size v + len)
      (fun bigstring ~pos:dst_pos ->
        let after_v = bin_writer.write bigstring ~pos:dst_pos v in
        Bigstring.blit ~src:buf ~src_pos:pos ~dst:bigstring
          ~dst_pos:after_v ~len)

  let send_bin_prot_and_bigstring_non_copying t bin_writer v ~buf ~pos ~len =
    match
      send_bin_prot_and_bigstring t bin_writer v ~buf ~pos ~len
    with
    | Sent () -> Rpc.Transport.Send_result.Sent (flushed t)
    | Closed -> Closed
    | Message_too_big too_big -> Message_too_big too_big
end
let ws_writer =
  (module WS_writer : Rpc_transport.Writer.S with type t = WS_writer.t)

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
              (addr :> Socket.Address.t)
          )
          >>| function
          | Error exn -> on_exn ~where:"WS.server" exn
          | Ok () -> ()
        in
        let bigstrings_from_client =
          Pipe.filter_map from_client ~f:(fun frame ->
            Log.Global.sexp ~level:`Debug [%message
              "arrived" (WS.Frame.show frame : string)
            ];
            match frame.opcode with
            | Close ->
              Pipe.write_without_pushback to_client (WS.Frame.close 1000);
              Pipe.close to_client;
              None
            | Binary -> Some (Bigstring.of_string frame.content)
            | _ -> None)
        in
        let bigstrings_to_client =
          Pipe.create_writer (fun reader ->
            Pipe.transfer reader to_client ~f:(fun bigstring ->
              let frame =
                WS.Frame.of_bytes ~opcode:Binary
                  (Bigstring.to_string bigstring)
              in
              Log.Global.sexp ~level:`Debug [%message
                "sent" (WS.Frame.show frame : string)
              ];
              frame
            )
          )
        in
        let f_result =
          let transport_reader =
            Rpc_transport.Reader.pack ws_reader
              { from_client = bigstrings_from_client
              ; unconsumed = ref (Bigsubstring.of_string "")
              }
          in
          let transport_writer =
            Rpc_transport.Writer.pack ws_writer
              { to_client = bigstrings_to_client
              ; monitor = Monitor.create ()
              }
          in
          { Rpc.Transport.reader = transport_reader
          ; writer = transport_writer
          } |> f addr
        in
        Deferred.any_unit [finished; f_result]
      )
    >>| fun _server ->
    Ok ())
