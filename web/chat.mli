open! Core_kernel
open Incr_dom
open Vdom

open Figgie

module Message : sig
  type t [@@deriving sexp_of]

  val horizontal_rule : Node.t
  val status : Node.t list -> t
  val error  : Node.t list -> t
  val simple : (Node.t list -> t) -> ('a, unit, string, t) format4 -> 'a

  val chat : username:Username.t -> is_me:bool -> msg:string -> t

  val player_event
    :  username:Username.t
    -> room_id:Lobby.Room.Id.t option
    -> event:Lobby.Room.Update.User_event.t
    -> t option
end

module Model : sig
  type t
  val initial : t
  val add_message : t -> Message.t -> t
end

module Action : sig
  type t =
    | Send_chat of string
    | Scroll_chat of Scrolling.Action.t
end

val apply_scrolling_action : Model.t -> Scrolling.Action.t -> Model.t

val view
  :  Model.t
  -> is_connected:bool
  -> inject:(Action.t -> Event.t)
  -> Node.t

val on_display
  :  old:Model.t
  -> Model.t
  -> schedule_scroll:(Scrolling.Action.t -> unit)
  -> unit
