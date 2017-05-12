open Async_rpc_kernel
open Incr_dom
open Figgie
open Market

module Model : sig
  type t

  val empty : t
end

val set_market : Model.t -> market:Book.t -> Model.t
val add_trade  : Model.t -> traded:Order.t -> with_:Cpty.t -> Model.t

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
  -> players:Username.Set.t
  -> inject:(Action.t -> Vdom.Event.t)
  -> Vdom.Node.t

val on_display
  :  Model.t
  -> schedule:(Action.t -> unit)
  -> unit
