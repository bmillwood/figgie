open Core.Std
open Async.Std

type t = {
  username : Username.t;
  conn : Rpc.Connection.t;
  updates : Protocol.Game_update.t Pipe.Reader.t;
  new_order_id : unit -> Market.Order.Id.t;
}

let run ~server ~room_id ~username ~f =
  Rpc.Connection.with_client
    ~host:(Host_and_port.host server)
    ~port:(Host_and_port.port server)
    (fun conn ->
      Rpc.Rpc.dispatch_exn Protocol.Login.rpc conn username
      >>= function
      | Error `Already_logged_in -> assert false
      | Ok () ->
        Rpc.Pipe_rpc.dispatch_exn Protocol.Join_room.rpc conn room_id
        >>= fun (updates, _metadata) ->
        Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc conn true
        >>= function
        | Error (`Not_logged_in | `Not_in_a_room | `Already_playing) ->
          assert false
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

let log_level_flag =
  let open Command.Param in
  flag "-log-level" (optional_with_default `Info Log.Level.arg)
    ~doc:"L Debug, Info, or Error"

let which_user ~stem i =
  stem ^ Option.value_map i ~default:"" ~f:Int.to_string
  |> Username.of_string

let make_command ~summary ~param ~username ~room_id ~f =
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
          ~room_id:(room_id stuff)
          ~f:(fun t -> f t stuff)
    ]
