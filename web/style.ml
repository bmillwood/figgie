open Core_kernel.Std
open Incr_dom
open Vdom

open Figgie

let suit_span ?(count=1) ~gold suit =
  let classes =
    Card.Suit.name suit
    :: (if Option.exists gold ~f:(Card.Suit.equal suit) then ["gold"] else [])
  in
  Node.span [Attr.classes classes]
    (List.init count ~f:(fun _ -> Icon.suit suit))
