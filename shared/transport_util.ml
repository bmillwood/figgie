open Core_kernel.Std
open Async_kernel.Std
open Async_rpc_kernel.Std

type 'a on_message =
  Bigstring.t -> pos:int -> len:int -> 'a Rpc.Transport.Handler_result.t

let read_forever_of_chunk_pipe chunk_pipe ~unconsumed ~is_closed =
  let read_n_bytes n ~on_end_of_batch =
    let unconsumed_len = Bigsubstring.length !unconsumed in
    if unconsumed_len >= n
    then begin
      let ret = Bigsubstring.sub !unconsumed ~pos:0 ~len:n in
      unconsumed :=
        Bigsubstring.sub !unconsumed ~pos:n ~len:(unconsumed_len - n);
      Deferred.return (Ok ret)
    end else begin
      let ret = Bigstring.create n in
      Bigsubstring.blit_to_bigstring !unconsumed ~dst:ret ~dst_pos:0;
      let rec loop ~dst_pos ~still_need =
        begin if Pipe.is_empty chunk_pipe
        then on_end_of_batch ()
        end;
        Pipe.read chunk_pipe
        >>= function
        | `Eof ->
          Deferred.return (Error (if is_closed () then `Closed else `Eof))
        | `Ok msg ->
          let len = Bigstring.length msg in
          if len >= still_need
          then begin
            Bigstring.blit
              ~src:msg ~src_pos:0 ~len:still_need ~dst:ret ~dst_pos;
            unconsumed := Bigsubstring.create ~pos:still_need msg;
            Deferred.return (Ok (Bigsubstring.create ret))
          end else begin
            Bigstring.blit ~src:msg ~src_pos:0 ~len ~dst:ret ~dst_pos;
            loop ~dst_pos:(dst_pos + len) ~still_need:(still_need - len)
          end
      in
      loop ~dst_pos:unconsumed_len ~still_need:(n - unconsumed_len)
    end
  in
  fun ~on_message ~on_end_of_batch ->
    Deferred.repeat_until_finished () (fun () ->
      begin
        read_n_bytes ~on_end_of_batch Rpc.Transport.Header.length
        >>=? fun header ->
        read_n_bytes ~on_end_of_batch
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
