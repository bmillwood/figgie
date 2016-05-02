open Core.Std

module Suit = struct
  type t = Spades | Hearts | Diamonds | Clubs
    [@@deriving bin_io, compare, enumerate, sexp]
  
  let equal t1 t2 = compare t1 t2 = 0

  let opposite = function
    | Spades -> Clubs
    | Clubs -> Spades
    | Hearts -> Diamonds
    | Diamonds -> Hearts

  let random_two () =
    let i1 = Random.int 4 in
    let i2 = (i1 + Random.int 3) mod 4 in
    let all = Array.of_list all in
    (all.(i1), all.(i2))
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
end
