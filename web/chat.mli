open! Core_kernel.Std
open Incr_dom
open Vdom

open Figgie
open Market

module Message : sig
  type t =
    | Connected_to_server of Host_and_port.t
    | Disconnected_from_server
    | Chat of { username : Username.t; is_me : bool; msg : string }
    | Chat_failed of [ `Chat_disabled | `Not_logged_in ]
    | Player_room_event of
        { username : Username.t
        ; room_id : Lobby.Room.Id.t option
        ; event : Lobby.Update.Player_event.t
        }
    | Joined_room of Lobby.Room.Id.t
    | New_round
    | Round_over of Protocol.Round_results.t
    | Order_reject of Order.t * Protocol.Order.error
    | Cancel_reject of [ `All | `Id of Order.Id.t ] * Protocol.Cancel.error
    [@@deriving sexp_of]
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
