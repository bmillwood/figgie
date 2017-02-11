open! Core.Std
open Async.Std

let param =
  let open Command.Param in
  flag "-which" (optional int)
    ~doc:"N modulate username"

let command =
  Bot.make_command
    ~summary:"Do nothing"
    ~param
    ~username:(fun i -> Bot.which_user ~stem:"lazybot" i)
    ~room_id:(fun _ -> Lobby.Room.Id.of_string "0")
    ~f:(fun client _which ->
      Pipe.iter client.updates ~f:(function
        | Broadcast (Round_over _) ->
          Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc client.conn true
          |> Deferred.ignore
        | _ -> Deferred.unit))
