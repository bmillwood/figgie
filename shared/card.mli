open Core_kernel.Std

module Suit : sig
  type t = Spades | Hearts | Diamonds | Clubs
    [@@deriving bin_io, compare, enumerate, sexp]

  val name : t -> string
  
  val equal : t -> t -> bool

  val opposite : t -> t

  val random_two : unit -> t * t
end

module Hand : sig
  type 'a t = { spades : 'a; hearts : 'a; diamonds : 'a; clubs : 'a }
    [@@deriving bin_io, sexp]

  val init : f:(Suit.t -> 'a) -> 'a t
  val create_all : 'a -> 'a t

  val get : 'a t -> suit:Suit.t -> 'a
  val set : 'a t -> suit:Suit.t -> to_:'a -> 'a t
  val modify : 'a t -> suit:Suit.t -> f:('a -> 'a) -> 'a t

  val map  : 'a t -> f:('a -> 'b) -> 'b t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val fold  : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val foldi : 'a t -> init:'b -> f:(Suit.t -> 'b -> 'a -> 'b) -> 'b
  val iter  : 'a t -> f:('a -> unit) -> unit

  val exists : 'a t -> f:('a -> bool) -> bool
end
