open Async_rpc_kernel
open Incr_dom
open Figgie
open Market

module Model : sig
  type t

  val empty : t
end

val set_market : Model.t -> market:Book.t -> Model.t

val exec : Model.t -> my_name:Username.t -> exec:Exec.t -> Model.t

module Action : sig
  type t [@@deriving sexp_of]
end

val apply_action
  :  Action.t
  -> Model.t
  -> my_name:Username.t
  -> conn:Rpc.Connection.t
  -> add_message:(Chat.Message.t -> unit)
  -> Model.t

val view
  :  Model.t
  -> my_name:Username.t
  -> shortener:Username.Shortener.t
  -> gold:Card.Suit.t option
  -> can_send_orders:bool
  -> inject:(Action.t -> Vdom.Event.t)
  -> Vdom.Node.t

val on_display
  :  old:Model.t option
  -> Model.t
  -> schedule:(Action.t -> unit)
  -> unit
