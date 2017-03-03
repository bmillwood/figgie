open! Core_kernel.Std
open Incr_dom
open Vdom

open Market

module Username_with_class : sig
  type t = Username.t * string [@@deriving sexp_of]
end

module Message : sig
  type t =
    | New_round
    | Round_over of Protocol.Round_results.t
    | Order_reject of Order.t * Protocol.Order.error
    | Cancel_reject of [ `All | `Id of Order.Id.t ] * Protocol.Cancel.error
    | Chat of Username_with_class.t * string
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
