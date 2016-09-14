type t = {
  known   : Market.Size.t Card.Hand.t;
  unknown : Market.Size.t;
}

let create_unknown size =
  { known = Card.Hand.create_all Market.Size.zero
  ; unknown = size
  }

let selling t ~suit ~size =
  let old_size = Card.Hand.get t.known ~suit in
  let open Market.Size.O in
  if size <= old_size
  then t
  else
    { known = Card.Hand.set t.known ~suit ~to_:size
    ; unknown = t.unknown - (size - old_size)
    }

let bought t ~suit ~size =
  let known = Card.Hand.modify t.known ~suit ~f:(Market.Size.(+) size) in
  { t with known }

let sold t ~suit ~size = bought t ~suit ~size:(Market.Size.neg size)
