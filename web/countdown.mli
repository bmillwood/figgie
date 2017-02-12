open Core_kernel.Std

module Model : sig
  type t
  val to_string : t -> string
end

module Action : sig
  type t [@@deriving sexp_of]

  val apply : t -> schedule:(t -> unit) -> Model.t -> Model.t
end

val of_end_time : schedule:(Action.t -> unit) -> Time_ns.t -> Model.t
