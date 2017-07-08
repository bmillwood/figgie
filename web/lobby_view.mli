open! Core_kernel.Std
open Incr_dom
open Vdom

open Figgie

module Action : sig
  module Room_action : sig
    type t =
      | Create
      | Join
      | Delete
  end

  type t = { room_id : Lobby.Room.Id.t; action : Room_action.t }
    [@@deriving sexp_of]
end

val view
  :  Lobby.t
  -> my_name:Username.t
  -> inject:(Action.t -> Event.t)
  -> Node.t
