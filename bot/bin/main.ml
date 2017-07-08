open Core

open Botlib

;;
Command.group
  ~summary:"Run a bot"
  [ "sell",  Sell.command
  ; "count", Count.command
  ; "chaos", Chaos.command
  ]
|> Command.run
