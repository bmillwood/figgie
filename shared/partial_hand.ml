open Market

type t =
  { known   : Size.t Card.Hand.t
  ; unknown : Size.t
  } [@@deriving bin_io, sexp]

let empty =
  { known = Card.Hand.create_all Size.zero
  ; unknown = Size.zero
  }

let is_empty t =
  Card.Hand.for_all t.known ~f:Size.(equal zero)
    && Size.(equal zero) t.unknown

let create_unknown size = { empty with unknown = size }

let create_known hand = { empty with known = hand }

let starting = create_unknown Params.num_cards_per_hand

let selling t ~suit ~size =
  let old_size = Card.Hand.get t.known ~suit in
  let open Size.O in
  if size <= old_size
  then t
  else
    { known = Card.Hand.set t.known ~suit ~to_:size
    ; unknown = t.unknown - (size - old_size)
    }

let bought t ~suit ~size =
  let known = Card.Hand.modify t.known ~suit ~f:(Size.(+) size) in
  { t with known }

let sold t ~suit ~size:size_sold =
  let known_sold = Size.min size_sold (Card.Hand.get t.known ~suit) in
  let unknown_sold = Size.O.(size_sold - known_sold) in
  let known =
    Card.Hand.modify t.known ~suit ~f:(fun s -> Size.O.(s - known_sold))
  in
  let unknown = Size.O.(t.unknown - unknown_sold) in
  { known; unknown }

let traded t ~suit ~size ~dir =
  Dir.fold dir ~buy:bought ~sell:sold t ~suit ~size

let apply_position_diff t ~suit ~diff =
  if Size.O.(diff < zero) then (
    sold t ~suit ~size:(Size.neg diff)
  ) else (
    bought t ~suit ~size:diff
  )

let apply_positions_diff t ~diff =
  Card.Hand.foldi diff ~init:t ~f:(fun suit t diff ->
      apply_position_diff t ~suit ~diff
    )
