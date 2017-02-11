open Core.Std

;;
Command.group
  ~summary:"Run a bot"
  [ "sell",  Sell.command
  ; "count", Count.command
  ]
|> Command.run
