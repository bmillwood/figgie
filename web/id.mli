open Core_kernel.Std
open Incr_dom
open Figgie

type t [@@deriving sexp]
include Comparable.S with type t := t

val of_elt : Dom_html.element Js.t -> t

val lookup_elt : t -> Dom_html.element Js.t option

val attr : t -> Vdom.Attr.t
val id   : t -> string

val container : t

val status     : t
val login      : t
val connect_to : t

val rooms       : t
val create_room : t

val ready_button : t

val exchange  : t
val market    : t
val suits_row : t
val order_row : dir:Market.Dir.t -> t
val order     : dir:Market.Dir.t -> suit:Card.Suit.t -> t
val tape      : t

val user_info : t
val others    : t

val chat    : t
val history : t
val cmdline : t
