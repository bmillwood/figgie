open Core_kernel.Std

open Market

module Persistent = struct
  type t = {
    username : Username.t;
    score : Price.t;
  } [@@deriving sexp]

  let nobody =
    { username = Username.of_string "[nobody]"
    ; score = Price.zero
    }
end

type t = {
  pers : Persistent.t;
  hand : Partial_hand.t;
}

let with_empty_hand pers = { pers; hand = Partial_hand.empty }

let nobody = with_empty_hand Persistent.nobody
