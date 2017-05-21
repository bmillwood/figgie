open Figgie
open Incr_dom
open Vdom

let suit (suit : Card.Suit.t) =
  Node.text (
    match suit with
    | Spades -> "\xe2\x99\xa0"
    | Hearts -> "\xe2\x99\xa5"
    | Diamonds -> "\xe2\x99\xa6"
    | Clubs -> "\xe2\x99\xa3"
  )

let unknown_suit = Node.text "\xe2\x96\x88"

let ready     = Node.text "\xe2\x9c\x93"
let not_ready = Node.text "\xf0\x9f\x9a\xab"
let delete    = Node.text "\xc3\x97"
let observer  = Node.text "\xf0\x9f\x91\x81"
