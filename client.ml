open Core.Std
open Async.Std

let main ~server ~username =
  let say s =
    Writer.(write_sexp ~hum:true ~terminate_with:Newline (Lazy.force stdout)) s
  in
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
        let order_id =
          let r = ref Market.Order.Id.zero in
          fun () ->
            let id = !r in
            r := Market.Order.Id.next id;
            id
        in
        Pipe.iter updates ~f:(fun update ->
          say [%sexp (update : Protocol.Update.t)];
          Random.self_init ();
          match update with
          | Dealt _hand ->
            let order symbol =
              { Market.Order.owner = username
              ; id = order_id ()
              ; symbol
              ; dir = if Random.int 2 = 0 then Sell else Buy
              ; price = Market.Price.of_int 5
              ; size = Market.Size.of_int 1
              }
            in
            Deferred.List.iter Card.Suit.all ~how:`Sequential ~f:(fun suit ->
              Rpc.Rpc.dispatch_exn Protocol.Order.rpc conn (order suit)
              >>| fun r ->
              say [%sexp (r : Protocol.Order.response)])
          | _ -> Deferred.unit
        ))

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
          ~username:(Username.of_string username)
        >>| Or_error.of_exn_result
    ]
