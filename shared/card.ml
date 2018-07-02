open Core_kernel

module Suit = struct
  type t = Spades | Hearts | Diamonds | Clubs
    [@@deriving bin_io, compare, enumerate, sexp]

  let name t = Sexp.to_string [%sexp (t : t)]

  let equal t1 t2 = compare t1 t2 = 0

  let opposite = function
    | Spades -> Clubs
    | Clubs -> Spades
    | Hearts -> Diamonds
    | Diamonds -> Hearts

  let random_two () =
    let i1 = Random.int 4 in
    let i2 = (i1 + Random.int 3 + 1) mod 4 in
    let all = Array.of_list all in
    (all.(i1), all.(i2))

  module Role = struct
    type t =
      | Normal
      | Short
      | Long
    [@@deriving enumerate, sexp]
  end

  let role t ~short ~long : Role.t =
    if equal t long then (
      Long
    ) else if equal t short then (
      Short
    ) else (
      Normal
    )
end

module Hand = struct
  type 'a t = { spades : 'a; hearts : 'a; diamonds : 'a; clubs : 'a }
    [@@deriving bin_io, sexp]

  let init ~(f : Suit.t -> _) =
    { spades = f Spades
    ; hearts = f Hearts
    ; diamonds = f Diamonds
    ; clubs = f Clubs
    }

  let create_all v = init ~f:(fun _ -> v)

  let get t ~suit =
    match (suit : Suit.t) with
    | Spades   -> t.spades
    | Hearts   -> t.hearts
    | Diamonds -> t.diamonds
    | Clubs    -> t.clubs

  let set t ~suit ~to_ =
    match (suit : Suit.t) with
    | Spades   -> { t with spades   = to_ }
    | Hearts   -> { t with hearts   = to_ }
    | Diamonds -> { t with diamonds = to_ }
    | Clubs    -> { t with clubs    = to_ }

  let modify t ~suit ~f =
    match (suit : Suit.t) with
    | Spades   -> { t with spades   = f t.spades   }
    | Hearts   -> { t with hearts   = f t.hearts   }
    | Diamonds -> { t with diamonds = f t.diamonds }
    | Clubs    -> { t with clubs    = f t.clubs    }

  let map t ~f = init ~f:(fun suit -> f (get t ~suit))
  let map2 t1 t2 ~f = init ~f:(fun suit -> f (get t1 ~suit) (get t2 ~suit))

  let foldi t ~init ~f =
    let f (suit : Suit.t) v a = f suit a v in
    init
    |> f Spades t.spades
    |> f Hearts t.hearts
    |> f Diamonds t.diamonds
    |> f Clubs t.clubs

  let fold t ~init ~f = foldi t ~init ~f:(fun _ b a -> f b a)

  let iter t ~f =
    f t.spades;
    f t.hearts;
    f t.diamonds;
    f t.clubs

  let exists t ~f =
    f t.spades || f t.hearts || f t.diamonds || f t.clubs

  let for_all t ~f =
    f t.spades && f t.hearts && f t.diamonds && f t.clubs
end
