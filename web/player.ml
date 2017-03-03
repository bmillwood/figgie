open Core_kernel.Std
open Incr_dom
open Vdom

open Market

module Style = struct
  type t =
    | Me
    | Them of int
    | Nobody
    [@@deriving compare, sexp]

  let equal t1 t2 = compare t1 t2 = 0

  let class_ =
    function
    | Me -> "me"
    | Them i -> "them" ^ Int.to_string i
    | Nobody -> "nobody"

  let style_text t text = Node.span [Attr.class_ (class_ t)] [Node.text text]
end

module Persistent = struct
  type t = {
    style : Style.t;
    username : Username.t;
    score : Price.t;
  } [@@deriving sexp]

  let nobody =
    { style = Nobody
    ; username = Username.of_string "[nobody]"
    ; score = Price.zero
    }

  let style_text t text = Style.style_text t.style text
end

type t = {
  pers : Persistent.t;
  hand : Partial_hand.t;
}

let with_empty_hand pers = { pers; hand = Partial_hand.empty }

let nobody = with_empty_hand Persistent.nobody
