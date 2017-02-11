open Core.Std

let length_of_round = Time_ns.Span.of_sec 10.

let gold_card_value = Market.Price.of_int 10
let pot_per_player ~num_players:_ = Market.Price.of_int 50

(* In a four-player game, the most you can possibly win from one card purchase
   is 100, when the gold suit has 8 cards so there's 120 in the pot, and you
   go from 2 cards each, 30 from the pot each, to one person having 3 and the
   whole pot -- gaining 90 from the pot and 10 from the card. *)
let validate_price price =
  if Market.Price.(price >= of_int 100) then (
    Error `Price_too_high
  ) else if Market.Price.(price <= of_int 0) then (
    Error `Price_must_be_nonnegative
  ) else (
    Ok ()
  )

let normal_suit_cards = Market.Size.of_int 10
let long_suit_cards = Market.Size.of_int 12
let short_suit_cards = Market.Size.of_int 8

let cards_in_suit suit ~long ~short =
  if Card.Suit.equal suit long
  then long_suit_cards
  else if Card.Suit.equal suit short
  then short_suit_cards
  else normal_suit_cards

let num_cards_in_deck =
  Market.Size.O.(2 * normal_suit_cards + long_suit_cards + short_suit_cards)
