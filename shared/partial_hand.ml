open Market

type t =
  { known   : Size.t Card.Hand.t
  ; unknown : Size.t
  } [@@deriving sexp_of]

let empty =
  { known = Card.Hand.create_all Size.zero
  ; unknown = Size.zero
  }

let create_unknown size = { empty with unknown = size }

let create_known hand = { empty with known = hand }

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

let unknown_utf8 = "\xe2\x96\x88"
