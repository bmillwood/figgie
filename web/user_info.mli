open! Core_kernel.Std
open Incr_dom
open Vdom

open Figgie

val empty : Node.t

val score_display
  :  ('k, Market.Price.t, _) Map.t
  -> Market.Price.t
  -> Node.t

val waiting
  :  inject_I'm_ready:(bool -> Event.t)
  -> users:Lobby.User.t Username.Map.t
  -> my_name:Username.t
  -> last_gold:Card.Suit.t option
  -> Node.t

val playing
  :  users:Lobby.User.t Username.Map.t
  -> my_name:Username.t
  -> Node.t
