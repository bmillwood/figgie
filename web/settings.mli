open Incr_dom
open Vdom

module Model : sig
  type t

  val initial : t

  val auto_cancel : t -> Auto_cancel.t
end

module Action : sig
  type t [@@deriving sexp_of]
end

val apply_action : Action.t -> Model.t -> Model.t

val view
  :  Model.t
  -> inject:(Action.t -> Event.t)
  -> Node.t
