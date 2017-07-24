open Market

type t =
  { known   : Size.t Card.Hand.t
  ; unknown : Size.t
  } [@@deriving bin_io, sexp]

val empty    : t
val starting : t

val is_empty : t -> bool

val create_unknown : Size.t             -> t
val create_known   : Size.t Card.Hand.t -> t

val selling : t -> suit:Card.Suit.t -> size:Size.t -> t
val traded  : t -> suit:Card.Suit.t -> size:Size.t -> dir:Dir.t -> t

val apply_position_diff  : t -> suit:Card.Suit.t -> diff:Size.t -> t
val apply_positions_diff : t -> diff:Size.t Card.Hand.t -> t
