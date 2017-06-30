open! Core
open Async

open Figgie
open Market

type t

val create : username:Username.t -> t

val reset : t -> unit

val new_order_id : t -> Order.Id.t

val send_order
  :  t
  -> conn:Rpc.Connection.t
  -> order:Order.t
  -> Protocol.Order.response Deferred.t

val exec : t -> exec:Exec.t   -> unit
val out  : t -> order:Order.t -> unit

val unacked_orders : t -> Order.t list
val open_orders    : t -> Order.t list Dirpair.t Per_symbol.t

val hand_if_no_fills : t -> Size.t Card.Hand.t option

val set_hand : t -> hand:Size.t Card.Hand.t -> unit
