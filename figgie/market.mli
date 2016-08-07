open Core.Std

module Cpty   = Username
module Symbol = Card.Suit

module Per_symbol : sig
  type 'a t = 'a Card.Hand.t

  val get    : 'a t -> symbol:Symbol.t -> 'a
  val set    : 'a t -> symbol:Symbol.t -> to_:'a       -> 'a t
  val modify : 'a t -> symbol:Symbol.t -> f:('a -> 'a) -> 'a t
end

module Dir : sig
  type t = Buy | Sell [@@deriving bin_io, compare, sexp]

  val equal : t -> t -> bool

  val other : t -> t

  val fold : t -> buy:'a -> sell:'a -> 'a
end

module Dirpair : sig
  type 'a t = { buy : 'a; sell : 'a } [@@deriving bin_io, sexp]

  val create_both : 'a -> 'a t
  val create_dir : dir:Dir.t -> same:'a -> opp:'a -> 'a t

  val get    : 'a t -> dir:Dir.t -> 'a
  val set    : 'a t -> dir:Dir.t -> to_:'a       -> 'a t
  val modify : 'a t -> dir:Dir.t -> f:('a -> 'a) -> 'a t

  val mapi : 'a t -> f:(Dir.t -> 'a -> 'b) -> 'b t
end

module type With_units = sig
  type t

  include Identifiable.S with type t := t

  val of_int : int -> t
  val to_float : t -> float

  module O : sig
    val zero : t
    val neg : t -> t
    val (+) : t -> t -> t
    val (-) : t -> t -> t
    val ( * ) : int -> t -> t
    val (/) : t -> int -> t
    val (=) : t -> t -> bool
    val (<>) : t -> t -> bool
    val (<) : t -> t -> bool
    val (>) : t -> t -> bool
    val (<=) : t -> t -> bool
    val (>=) : t -> t -> bool
  end
  include module type of O
end

module Price : sig
  include With_units
  val more_agg          : t -> than:t -> dir:Dir.t -> bool
  val more_agg_or_equal : t -> than:t -> dir:Dir.t -> bool
end

module Size : sig 
  include With_units
  val to_int : t -> int
  val with_dir : t -> dir:Dir.t -> t
end

module O : sig
  val ( *$ ) : Size.t -> Price.t -> Price.t
  module Price = Price.O
  module Size = Size.O
end
val ( *$ ) : Size.t -> Price.t -> Price.t

module Order : sig
  module Id : sig
    include Identifiable.S
    val zero : t
    val next : t -> t
  end

  type t = {
    owner : Cpty.t;
    id : Id.t;
    symbol : Symbol.t;
    dir : Dir.t;
    price : Price.t;
    size : Size.t;
  } [@@deriving bin_io, sexp]

  val more_agg_or_equal : t -> than:t -> bool
  val more_agg          : t -> than:t -> bool
end

module Exec : sig
  module Partial_fill : sig
    type t = {
      original_order : Order.t;
      filled_by : Size.t;
    } [@@deriving bin_io, sexp]
  end

  type t = {
    fully_filled     : Order.t list;
    partially_filled : Partial_fill.t option;
    posted           : Order.t option;
  } [@@deriving bin_io, sexp]
end

module Match_result : sig
  type 'a t = { exec : Exec.t; remaining : 'a }
end

module Halfbook : sig
  type t = Order.t list [@@deriving bin_io, sexp]
end

module Book : sig
  type t = Halfbook.t Dirpair.t Per_symbol.t [@@deriving bin_io, sexp]

  val empty : t
  val match_ : t -> Order.t -> t Match_result.t
  val cancel : t -> Order.t -> (t, [> `No_such_order ]) Result.t
end
