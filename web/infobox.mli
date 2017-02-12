open! Core_kernel.Std
open Incr_dom.Std
open Vdom

val empty : Node.t

val login : inject_login:(Username.t -> Event.t) -> Node.t

val waiting
  :  inject_I'm_ready:(bool -> Event.t)
  -> me:Player.Persistent.t
  -> others:Player.Persistent.t Username.Map.t
  -> who_is_ready:Username.Set.t
  -> Node.t

val playing
  :  others:Player.t Username.Map.t
  -> me:Player.t
  -> Node.t
