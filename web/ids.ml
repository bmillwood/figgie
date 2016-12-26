let login = "login"
let connectTo = "connectTo"
let ready_button = "readyButton"

let order ~dir ~suit =
  "order" ^ Market.Dir.to_string dir ^ Card.Suit.name suit

let cancel = "cxl"
let tape = "tape"

let cmdline = "cmdline"
