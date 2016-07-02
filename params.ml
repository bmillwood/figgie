open Core.Std

let min_players = 4
let max_players = 4

let length_of_round = Time.Span.of_min 1.

let gold_card_value = Market.Price.of_int 10
let pot_per_player ~num_players:_ = Market.Price.of_int 50

let normal_suit_cards = Market.Size.of_int 10
let long_suit_cards = Market.Size.of_int 12
let short_suit_cards = Market.Size.of_int 8
let num_cards_in_deck =
  Market.Size.O.(2 * normal_suit_cards + long_suit_cards + short_suit_cards)
