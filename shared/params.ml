open Core_kernel

open Market

let gold_card_value = Price.of_int 10
let pot_per_player = Price.of_int 50
let num_players = 4
let pot = Size.of_int num_players *$ pot_per_player

(* In a four-player game, the most you can possibly win from one card purchase
   is 100, when the gold suit has 8 cards so there's 120 in the pot, and you
   go from 2 cards each, 30 from the pot each, to one person having 3 and the
   whole pot -- gaining 90 from the pot and 10 from the card. *)
let validate_price price =
  if Price.(price >= of_int 100) then (
    Error `Price_too_high
  ) else if Price.(price <= of_int 0) then (
    Error `Price_must_be_nonnegative
  ) else (
    Ok ()
  )

let num_cards_in_role (role : Card.Suit.Role.t) =
  let num =
    match role with
    | Normal -> 10
    | Long   -> 12
    | Short  -> 8
  in
  Size.of_int num

let num_cards_in_deck = Size.of_int 40

let num_cards_per_hand = Size.of_int 10
