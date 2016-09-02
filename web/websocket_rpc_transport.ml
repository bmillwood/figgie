open Async_rpc_kernel.Std
open Async_kernel.Std
open Core_kernel.Std

module T = struct
  type t = {
    socket : WebSockets.webSocket Js.t;
    closed : unit Deferred.t;
    monitor : Monitor.t;
    incoming_messages : Bigstring.t Pipe.Reader.t;
    close_started : bool ref;
    unconsumed_message : Bigsubstring.t ref;
  }

  let of_socket ~host_and_port:_ socket =
    let close_started = ref false in
    let closed =
      Deferred.create (fun closed_ivar ->
        socket##.onclose := Dom.handler (fun _event ->
          Ivar.fill closed_ivar ();
          close_started := true;
          Js._false))
    in
    let incoming_messages =
      Pipe.create_reader ~close_on_exception:false (fun writer ->
        socket##.onmessage := Dom.handler (fun event ->
          Js.Unsafe.global##.debugEvent := event;
          let bigstring =
            Typed_array.Bigstring.of_arrayBuffer event##.data_buffer
          in
          Incr_dom.Common.logf "msg arrived %S"
            (Bigstring.to_string bigstring);
          Pipe.write_without_pushback writer bigstring;
          Js._false);
        closed)
    in
    { socket
    ; closed
    ; monitor = Monitor.create ()
    ; incoming_messages
    ; close_started
    ; unconsumed_message = ref (Bigsubstring.of_string "")
    }

  let sexp_of_t _t = Sexp.Atom "<websocket>"

  let is_closed t = !(t.close_started)

  let close t =
    t.close_started := true;
    t.socket##close;
    t.closed

  let read_forever t =
    Transport_util.read_forever_of_chunk_pipe
      t.incoming_messages
      ~unconsumed:t.unconsumed_message
      ~is_closed:(fun () -> is_closed t)

  let monitor t = t.monitor
  let bytes_to_write _t = 0
  let stopped t = t.closed
  let flushed _t = Deferred.unit
  let ready_to_write = flushed

  let send_bigstring t len write_bigstring =
    if is_closed t
    then Rpc.Transport.Send_result.Closed
    else begin
      let bigstring = Bigstring.create (len + Rpc.Transport.Header.length) in
      Rpc.Transport.Header.unsafe_set_payload_length bigstring ~pos:0 len;
      let _pos = write_bigstring bigstring ~pos:Rpc.Transport.Header.length in
      t.socket##send_buffer (Typed_array.Bigstring.to_arrayBuffer bigstring);
      Incr_dom.Common.logf "msg sent %S"
        (Bigstring.to_string bigstring);
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

let reader = (module T : Rpc.Transport.Reader.S with type t = T.t)
let writer = (module T : Rpc.Transport.Writer.S with type t = T.t)

let connect host_and_port =
  let socket =
    let host, port = Host_and_port.tuple host_and_port in
    new%js WebSockets.webSocket
      (Js.string (sprintf "ws://%s:%d" host port))
  in
  socket##.binaryType := Js.string "arraybuffer";
  Deferred.create (fun ivar ->
    socket##.onopen := Dom.handler (fun _event ->
      let transport_reader =
        Rpc.Transport.Reader.pack reader (T.of_socket ~host_and_port socket)
      in
      let transport_writer =
        Rpc.Transport.Writer.pack writer (T.of_socket ~host_and_port socket)
      in
      Ivar.fill_if_empty ivar
        (Ok { Rpc.Transport.reader = transport_reader
        ; writer = transport_writer
        });
      Js._false
    );
    socket##.onerror := Dom.handler (fun _event ->
      Ivar.fill_if_empty ivar (Error ());
      Js._false
    )
  )
