open Core_kernel.Std

include Identifiable.S

module Shortener : sig
  type t
  type username

  val of_list : username list -> t
  val short   : t -> username -> username
end with type username := t
