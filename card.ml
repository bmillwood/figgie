open Core.Std

module Suit = struct
  type t = Spades | Hearts | Diamonds | Clubs [@@deriving bin_io, compare, enumerate]
  
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

  let map t ~f = init ~f:(fun suit -> f (get t ~suit))

  (* this is dumb. *)
  let deal ~num_players =
    let long, short = Suit.random_two () in
    let deck = init ~f:(fun _ -> ref 10) in
    get deck ~suit:long := 12;
    get deck ~suit:short := 8;
    let remaining_suits = ref Suit.all in
    let hands = Array.init num_players ~f:(fun _i -> init ~f:(fun _ -> ref 0)) in
    for ix = 0 to 39 do
      let suit =
        List.nth_exn !remaining_suits (Random.int (List.length !remaining_suits))
      in
      decr (get deck ~suit);
      incr (get hands.(ix mod num_players) ~suit);
      if !(get deck ~suit) = 0
      then remaining_suits := List.filter !remaining_suits ~f:(Fn.non (Suit.equal suit))
    done;
    Array.map hands ~f:(map ~f:(fun r -> !r))
end
