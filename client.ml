open Core.Std
open Async.Std

type t = {
  username : Username.t;
  conn : Rpc.Connection.t;
  updates : Protocol.Update.t Pipe.Reader.t;
  new_order_id : unit -> Market.Order.Id.t;
}

let run ~server ~username ~f =
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
        let new_order_id =
          let r = ref Market.Order.Id.zero in
          fun () ->
            let id = !r in
            r := Market.Order.Id.next id;
            id
        in
        Pipe.iter updates ~f:(function
          | Round_over _ ->
            Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc conn true
            |> Deferred.ignore
          | _ -> Deferred.unit)
        |> don't_wait_for;
        f { username; conn; updates; new_order_id })
  >>| Or_error.of_exn_result

let make_command ~summary ~param ~username ~f =
  let open Command.Let_syntax in
  Command.async_or_error'
    ~summary
    [%map_open
      let server = anon ("HOST:PORT" %: string)
      and stuff = param
      in
      fun () ->
        run
          ~server:(Host_and_port.of_string server)
          ~username:(username stuff)
          ~f:(fun t -> f t stuff)
    ]
