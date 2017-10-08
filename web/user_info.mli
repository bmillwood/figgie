open! Core_kernel.Std
open Async_rpc_kernel
open Incr_dom
open Vdom

open Figgie

module Model : sig
  type t

  val initial : t
end

module Action : sig
  type t [@@deriving sexp_of]
end

val score_display
  :  all_scores:Market.Price.t list
  -> Market.Price.t
  -> Node.t

val apply_action
  :  Action.t
  -> Model.t
  -> conn:Rpc.Connection.t
  -> room_id:Lobby.Room.Id.t
  -> room:Lobby.Room.t
  -> my_name:Username.t
  -> Model.t

val view
  :  Model.t
  -> inject:(Action.t -> Event.t)
  -> room:Lobby.Room.t
  -> my_hand:Partial_hand.t
  -> my_name:Username.t
  -> gold:Card.Suit.t option
  -> Node.t
