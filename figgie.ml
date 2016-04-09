open! Core.Std
open Async.Std

;;
Command.group
  ~summary:"Figgie!"
  [ "client", Client.command
  ; "server", Server.command
  ]
|> Command.run
