open! Core.Std
open Async.Std

;;
Command.group
  ~summary:"Figgie!"
  [ "bot", Bot.command
  ; "manual", Manual.command
  ; "server", Server.command
  ]
|> Command.run
