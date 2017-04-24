open Market

type t =
  { known   : Size.t Card.Hand.t
  ; unknown : Size.t
  } [@@deriving bin_io, sexp]

val empty : t

val create_unknown : Size.t             -> t
val create_known   : Size.t Card.Hand.t -> t

val selling : t -> suit:Card.Suit.t -> size:Size.t -> t
val bought  : t -> suit:Card.Suit.t -> size:Size.t -> t
val sold    : t -> suit:Card.Suit.t -> size:Size.t -> t
val traded  : t -> suit:Card.Suit.t -> size:Size.t -> dir:Dir.t -> t
