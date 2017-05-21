open Core_kernel.Std
open Incr_dom
open Figgie

include String

let of_elt (elt : Dom_html.element Js.t) = Js.to_string elt##.id

let lookup_elt t =
  match Dom_html.getElementById t with
  | exception Not_found -> None
  | elt -> Some elt

let attr t = Vdom.Attr.id t
let id   t = t

let container = "container"

let status     = "status"
let login      = "login"
let connect_to = "connectTo"

let rooms = "rooms"

let ready_button = "readyButton"

let exchange = "exchange"
let market   = "market"

let suits_row = "suits"

let order_row ~dir       = "order" ^ Market.Dir.to_string dir
let order     ~dir ~suit = order_row ~dir ^ Card.Suit.name suit

let tape = "tape"

let user_info = "userinfo"
let others    = "others"

let chat    = "historycmd"
let cmdline = "cmdline"
let history = "history"
