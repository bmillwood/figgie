open Core_kernel.Std

open Figgie
open Market

type t =
  { username : Username.t
  ; is_connected : bool
  ; score : Price.t
  ; hand : Partial_hand.t
  } [@@deriving sexp]

let nobody =
  { username = Username.of_string "[nobody]"
  ; is_connected = true
  ; score = Price.zero
  ; hand = Partial_hand.empty
  }
