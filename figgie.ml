open! Core.Std
open Async.Std

;;
Command.group
  ~summary:"Figgie!"
  [ "bot", Bot.command
  ; "server", Server.command
  ]
|> Command.run
