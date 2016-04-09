open Core.Std
open Async.Std

let main ~server ~username =
  Rpc.Connection.with_client
    ~host:(Host_and_port.host server)
    ~port:(Host_and_port.port server)
    (fun conn ->
      Rpc.Pipe_rpc.dispatch_exn Protocol.Join_game.rpc conn username
      >>= fun (updates, _metadata) ->
      Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc conn true
      >>= function
      | Error `Already_playing -> assert false
      | Ok () ->
        Pipe.iter updates ~f:(fun update ->
          Writer.(write_sexp ~hum:true ~terminate_with:Newline (Lazy.force stdout))
            (Protocol.Update.sexp_of_t update);
          Deferred.unit))

let command =
  let open Command.Let_syntax in
  Command.async_or_error'
    ~summary:"Figgie client"
    [%map_open
      let server = anon ("HOST:PORT" %: string)
      and username = flag "-username" (required string) ~doc:"NAME"
      in
      fun () ->
        main
          ~server:(Host_and_port.of_string server)
          ~username:(Protocol.Username.of_string username)
        >>| Or_error.of_exn_result
    ]
