open Core.Std
open Async.Std

type t = {
  username : Username.t;
  conn : Rpc.Connection.t;
  updates : Protocol.Player_update.t Pipe.Reader.t;
  new_order_id : unit -> Market.Order.Id.t;
}

let run ~server ~username ~f =
  Rpc.Connection.with_client
    ~host:(Host_and_port.host server)
    ~port:(Host_and_port.port server)
    (fun conn ->
      Rpc.Pipe_rpc.dispatch_exn Protocol.Login.rpc conn username
      >>= fun (updates, _metadata) ->
      Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc conn true
      >>= function
      | Error (`Login_first | `Already_playing) -> assert false
      | Ok () ->
        let new_order_id =
          let r = ref Market.Order.Id.zero in
          fun () ->
            let id = !r in
            r := Market.Order.Id.next id;
            id
        in
        f { username; conn; updates; new_order_id })
  >>| Or_error.of_exn_result

let make_command ~summary ~param ~username ~f =
  let open Command.Let_syntax in
  Command.async_or_error'
    ~summary
    [%map_open
      let server =
        flag "-server" (required string)
          ~doc:"HOST:PORT where to connect"
      and stuff = param
      in
      fun () ->
        run
          ~server:(Host_and_port.of_string server)
          ~username:(username stuff)
          ~f:(fun t -> f t stuff)
    ]
