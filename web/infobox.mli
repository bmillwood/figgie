open! Core_kernel.Std
open Incr_dom
open Vdom

open Figgie

val empty : Node.t

val waiting
  :  inject_I'm_ready:(bool -> Event.t)
  -> me:Player.t
  -> others:Player.t Username.Map.t
  -> who_is_ready:Username.Set.t
  -> Node.t

val playing
  :  others:Player.t Username.Map.t
  -> me:Player.t
  -> Node.t
