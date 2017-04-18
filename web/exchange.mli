open Core_kernel
open Async_rpc_kernel.Std
open Incr_dom
open Figgie
open Market

module Model : sig
  type t

  val initial : t
end

module Cancel_scope : sig
  type t =
    | All
    | By_id of Order.Id.t
    | By_symbol_side of { symbol : Symbol.t; dir : Dir.t }
    [@@deriving sexp_of]
end

module Action : sig
  type t =
    | Send_order of
        { symbol : Card.Suit.t
        ; dir    : Dir.t
        ; price  : Price.t
        }
    | Send_cancel of Cancel_scope.t
    [@@deriving sexp_of]
end

val apply_action
  :  Action.t
  -> Model.t
  -> my_name:Username.t
  -> conn:Rpc.Connection.t
  -> market:Book.t
  -> add_message:(Chat.Message.t -> unit)
  -> Model.t

val view
  :  my_name:Username.t
  -> market:Book.t
  -> trades:(Order.t * Username.t) Fqueue.t
  -> players:Username.Set.t
  -> inject:(Action.t -> Vdom.Event.t)
  -> Vdom.Node.t
