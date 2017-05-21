open Core_kernel.Std
open Incr_dom
open Vdom

open Figgie

let suit_utf8 (suit : Card.Suit.t) =
  match suit with
  | Spades -> "\xe2\x99\xa0"
  | Hearts -> "\xe2\x99\xa5"
  | Diamonds -> "\xe2\x99\xa6"
  | Clubs -> "\xe2\x99\xa3"

let suit_span ?(count=1) ~gold suit =
  let classes =
    Card.Suit.name suit
    :: (if Option.exists gold ~f:(Card.Suit.equal suit) then ["gold"] else [])
  in
  let text =
    let utf8 = suit_utf8 suit in
    String.concat (List.init count ~f:(fun _ -> utf8))
  in
  Node.span [Attr.classes classes]
    [Node.text text]
