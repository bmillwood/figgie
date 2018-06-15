open! Core_kernel
open Incr_dom

val char_code : Dom_html.keyboardEvent Js.t -> char option

module Global : sig
  val placeholder_of_id : Id.t -> string option

  val lookup_id : Id.t -> char option

  val handler : Vdom.Attr.t
end
