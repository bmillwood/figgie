open Core.Std

;;
Command.group
  ~summary:"Run a bot"
  [ "lazy",  Lazy_.command
  ; "sell",  Sell.command
  ; "count", Count.command
  ]
|> Command.run
