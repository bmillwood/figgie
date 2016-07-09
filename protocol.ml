open Core.Std
open Async.Std

module Update = struct
  type t =
    | Player_joined of Username.t
    | Chat of Username.t * string
    | Waiting_for of int
    | Dealt of Market.Size.t Card.Hand.t
    | Exec of Market.Order.t * Market.Exec.t
    | Out of Market.Order.t
    | Round_over of Game.Round.Results.t
    [@@deriving bin_io, sexp]
end

module Join_game = struct
  type query = Username.t [@@deriving bin_io, sexp]
  type error = [ `Game_is_full | `Game_already_started ]
    [@@deriving bin_io, sexp]
  type response = Update.t [@@deriving bin_io, sexp]
  let rpc =
    Rpc.Pipe_rpc.create
      ~name:"join" ~version:1 ~bin_query ~bin_response ~bin_error
      ()
end

module Is_ready = struct
  type query = bool [@@deriving bin_io, sexp]
  type response = (unit, [ `Already_playing ]) Result.t
    [@@deriving bin_io, sexp]
  let rpc = Rpc.Rpc.create ~name:"ready" ~version:1 ~bin_query ~bin_response
end

module Chat = struct
  type msg = string [@@deriving bin_io, sexp]
  let rpc = Rpc.One_way.create ~name:"chat" ~version:1 ~bin_msg
end

module Hand = struct
  type query = unit [@@deriving bin_io, sexp]
  type response =
    ( Market.Size.t Card.Hand.t * Market.Price.t
    , [ `You're_not_playing
      | `Game_not_in_progress
      ]
    ) Result.t [@@deriving bin_io, sexp]
  let rpc = Rpc.Rpc.create ~name:"hand" ~version:1 ~bin_query ~bin_response
end

module Book = struct
  type query = unit [@@deriving bin_io, sexp]
  type response = Market.t [@@deriving bin_io, sexp]
  let rpc = Rpc.Rpc.create ~name:"book" ~version:1 ~bin_query ~bin_response
end

module Order = struct
  type query = Market.Order.t [@@deriving bin_io, sexp]
  type error =
    [ `You're_not_playing
    | `Game_not_in_progress
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
    [ `You're_not_playing
    | `Game_not_in_progress
    | `No_such_order
    ] [@@deriving bin_io, sexp]
  type response = (unit, error) Result.t [@@deriving bin_io, sexp]
  let rpc = Rpc.Rpc.create ~name:"cancel" ~version:1 ~bin_query ~bin_response
end
