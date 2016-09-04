open Core_kernel.Std
open Async_rpc_kernel.Std

module Round_results = struct
  type t = {
    gold : Card.Suit.t;
    hands : Market.Size.t Card.Hand.t Username.Map.t;
    scores_this_round : Market.Price.t Username.Map.t;
  } [@@deriving bin_io, sexp]
end

module Broadcast = struct
  type t =
    | Player_joined of Username.t
    | Chat of Username.t * string
    | Waiting_for of int
    | Exec of Market.Order.t * Market.Exec.t
    | Out of Market.Order.t
    | Round_over of Round_results.t
    [@@deriving bin_io, sexp]
end

module Player_update = struct
  type t =
    | Broadcast of Broadcast.t
    | Dealt of Market.Size.t Card.Hand.t
    [@@deriving bin_io, sexp]
end

module Web_update = struct
  type t =
    | Broadcast of Broadcast.t
    | Hands of Market.Size.t Card.Hand.t Username.Map.t
    | Market of Market.Book.t
    [@@deriving bin_io, sexp]
end

module Get_web_updates = struct
  type query = unit [@@deriving bin_io, sexp]
  type error = Nothing.t [@@deriving bin_io, sexp]
  type response = Web_update.t [@@deriving bin_io, sexp]
  let rpc =
    Rpc.Pipe_rpc.create
      ~name:"web-updates" ~version:1 ~bin_query ~bin_response ~bin_error
      ()
end

module Login = struct
  type query = Username.t [@@deriving bin_io, sexp]
  type error = [ `Game_is_full | `Game_already_started | `Already_logged_in ]
    [@@deriving bin_io, sexp]
  type response = Player_update.t [@@deriving bin_io, sexp]
  let rpc =
    Rpc.Pipe_rpc.create
      ~name:"login" ~version:1 ~bin_query ~bin_response ~bin_error
      ()
end

module Is_ready = struct
  type query = bool [@@deriving bin_io, sexp]
  type response = (unit, [ `Login_first | `Already_playing ]) Result.t
    [@@deriving bin_io, sexp]
  let rpc = Rpc.Rpc.create ~name:"ready" ~version:1 ~bin_query ~bin_response
end

module Chat = struct
  type query = string [@@deriving bin_io, sexp]
  type response = (unit, [ `Login_first ]) Result.t
    [@@deriving bin_io, sexp]
  let rpc = Rpc.Rpc.create ~name:"chat" ~version:1 ~bin_query ~bin_response
end

module Hand = struct
  type query = unit [@@deriving bin_io, sexp]
  type response =
    ( Market.Size.t Card.Hand.t * Market.Price.t
    , [ `Login_first
      | `Game_not_in_progress
      | `You're_not_playing
      ]
    ) Result.t [@@deriving bin_io, sexp]
  let rpc = Rpc.Rpc.create ~name:"hand" ~version:1 ~bin_query ~bin_response
end

module Book = struct
  type query = unit [@@deriving bin_io, sexp]
  type response = Market.Book.t [@@deriving bin_io, sexp]
  let rpc = Rpc.Rpc.create ~name:"book" ~version:1 ~bin_query ~bin_response
end

module Order = struct
  type query = Market.Order.t [@@deriving bin_io, sexp]
  type error =
    [ `Login_first
    | `Game_not_in_progress
    | `You're_not_playing
    | `Owner_is_not_sender
    | `Duplicate_order_id
    | `Price_must_be_nonnegative
    | `Size_must_be_positive
    | `Not_enough_to_sell
    ] [@@deriving bin_io, sexp]
  type response = (Market.Exec.t, error) Result.t [@@deriving bin_io, sexp]
  let rpc = Rpc.Rpc.create ~name:"order" ~version:1 ~bin_query ~bin_response
end

module Cancel = struct
  type query = Market.Order.Id.t [@@deriving bin_io, sexp]
  type error =
    [ `Login_first
    | `Game_not_in_progress
    | `You're_not_playing
    | `No_such_order
    ] [@@deriving bin_io, sexp]
  type response = (unit, error) Result.t [@@deriving bin_io, sexp]
  let rpc = Rpc.Rpc.create ~name:"cancel" ~version:1 ~bin_query ~bin_response
end
