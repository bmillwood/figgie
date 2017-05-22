open! Core_kernel.Std
open Incr_dom
open Vdom

open Figgie

val score_display
  :  ('k, Market.Price.t, _) Map.t
  -> Market.Price.t
  -> Node.t

val view
  :  inject_I'm_ready:(bool -> Event.t)
  -> users:Lobby.User.t Username.Map.t
  -> my_name:Username.t
  -> gold:Card.Suit.t option
  -> Node.t
