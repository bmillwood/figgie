open! Core.Std
open Async.Std

let () = Random.self_init ()
;;
Command.group
  ~summary:"Figgie!"
  [ "bot", Bot.command
  ; "manual", Manual.command
  ; "server", Server.command
  ]
|> Command.run
