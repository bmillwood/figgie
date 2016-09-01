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
    mutable unconsumed_message : Bigsubstring.t;
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
          let bigstring =
            Typed_array.Bigstring.of_arrayBuffer event##.data_buffer
          in
          Pipe.write_without_pushback writer bigstring;
          Js._false);
        closed)
    in
    { socket
    ; closed
    ; monitor = Monitor.create ()
    ; incoming_messages
    ; close_started
    ; unconsumed_message = Bigsubstring.of_string ""
    }

  let sexp_of_t _t = Sexp.Atom "<websocket>"

  let is_closed t = !(t.close_started)

  let close t =
    t.close_started := true;
    t.socket##close;
    t.closed

  let read_n_bytes t n ~on_end_of_batch =
    let unconsumed_len = Bigsubstring.length t.unconsumed_message in
    if unconsumed_len >= n
    then begin
      let ret = Bigsubstring.sub t.unconsumed_message ~pos:0 ~len:n in
      t.unconsumed_message <-
        Bigsubstring.sub t.unconsumed_message ~pos:n ~len:(unconsumed_len - n);
      Deferred.return (Ok ret)
    end else begin
      let ret = Bigstring.create n in
      Bigsubstring.blit_to_bigstring t.unconsumed_message ~dst:ret ~dst_pos:0;
      let rec loop ~dst_pos ~still_need =
        begin if Pipe.is_empty t.incoming_messages
        then on_end_of_batch ()
        end;
        Pipe.read t.incoming_messages
        >>= function
        | `Eof ->
          Deferred.return (Error (if is_closed t then `Closed else `Eof))
        | `Ok msg ->
          let len = Bigstring.length msg in
          if len >= still_need
          then begin
            Bigstring.blit ~src:msg ~src_pos:0 ~len:still_need ~dst:ret ~dst_pos;
            t.unconsumed_message <- Bigsubstring.create ~pos:still_need msg;
            Deferred.return (Ok (Bigsubstring.create ret))
          end else begin
            Bigstring.blit ~src:msg ~src_pos:0 ~len ~dst:ret ~dst_pos;
            loop ~dst_pos:(dst_pos + len) ~still_need:(still_need - len)
          end
      in
      loop ~dst_pos:unconsumed_len ~still_need:(n - unconsumed_len)
    end

  let read_forever t ~on_message ~on_end_of_batch =
    Deferred.repeat_until_finished () (fun () ->
      begin
        read_n_bytes t ~on_end_of_batch Rpc.Transport.Header.length
        >>=? fun header ->
        read_n_bytes t ~on_end_of_batch
          (Rpc.Transport.Header.unsafe_get_payload_length
            (Bigsubstring.base header)
            ~pos:(Bigsubstring.pos header))
        >>=? fun message ->
        match
          on_message
            (Bigsubstring.base message)
            ~pos:(Bigsubstring.pos message)
            ~len:(Bigsubstring.length message)
        with
        | Rpc.Transport.Handler_result.Stop a ->
            Deferred.return (Ok (`Finished (Ok a)))
        | Continue ->
            Deferred.return (Ok (`Repeat ()))
        | Wait wait ->
            wait >>| fun () -> Ok (`Repeat ())
      end
      >>| function
      | Error e -> `Finished (Error e)
      | Ok v -> v
    )

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
      let _pos = write_bigstring bigstring ~pos:Rpc.Transport.Header.length in
      Rpc.Transport.Header.unsafe_set_payload_length bigstring ~pos:0 len;
      t.socket##send_buffer (Typed_array.Bigstring.to_arrayBuffer bigstring);
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
      Js._false))
