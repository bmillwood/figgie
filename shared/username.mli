open Core_kernel

include Identifiable.S

val is_valid : t -> bool

module Shortener : sig
  type t
  type username

  val of_list : username list -> t
  val short   : t -> username -> username
end with type username := t
