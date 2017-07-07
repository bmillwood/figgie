open Core_kernel.Std
open Async_rpc_kernel

let default_async_rpc_port = 58828
let default_websocket_port = 58829

module Login = struct
  type query = Username.t [@@deriving bin_io, sexp]
  type response =
    ( unit
    , [ `Invalid_username | `Already_logged_in ]
    ) Result.t [@@deriving bin_io, sexp]
  let rpc =
    Rpc.Rpc.create
      ~name:"login" ~version:1 ~bin_query ~bin_response
end

module Lobby_update = struct
  type t =
    | Lobby_snapshot of Lobby.t
    | Lobby_update of Lobby.Update.t
    | Chat of Username.t * string
    [@@deriving bin_io, sexp]
end

module Get_lobby_updates = struct
  type query = unit [@@deriving bin_io, sexp]
  type response = Lobby_update.t [@@deriving bin_io, sexp]
  type error = [ `Not_logged_in ] [@@deriving bin_io, sexp]
  let rpc =
    Rpc.Pipe_rpc.create
      ~name:"get_lobby_updates" ~version:1
      ~bin_query ~bin_response ~bin_error
      ()
end

module Round_results = struct
  type t = {
    gold : Card.Suit.t;
    scores_this_round : Market.Price.t Username.Map.t;
  } [@@deriving bin_io, sexp]
end

module Broadcast = struct
  type t =
    | Room_update of Lobby.Room.Update.t
    | Chat of Username.t * string
    | Exec of Market.Exec.t
    | Out of Market.Order.t
    | Round_over of Round_results.t
    [@@deriving bin_io, sexp]
end

module Game_update = struct
  type t =
    | Room_snapshot of Lobby.Room.t
    | Broadcast of Broadcast.t
    | Hand of Market.Size.t Card.Hand.t
    | Market of Market.Book.t
    [@@deriving bin_io, sexp]
end

type not_logged_in = [ `Not_logged_in ] [@@deriving bin_io, sexp]

module Join_room = struct
  type query = Lobby.Room.Id.t [@@deriving bin_io, sexp]
  type response = Game_update.t [@@deriving bin_io, sexp]
  type error =
    [ not_logged_in
    | `Already_in_a_room
    | `No_such_room
    ] [@@deriving bin_io, sexp]

  let rpc =
    Rpc.Pipe_rpc.create
      ~name:"join-room" ~version:1
      ~bin_query
      ~bin_response
      ~bin_error
      ()
end

module Create_room = struct
  type query = Lobby.Room.Id.t [@@deriving bin_io, sexp]
  type error =
    [ `Room_already_exists
    | `Invalid_room_name
    ] [@@deriving bin_io, sexp]
  type response = (unit, error) Result.t [@@deriving bin_io, sexp]

  let rpc =
    Rpc.Rpc.create ~name:"create-room" ~version:1 ~bin_query ~bin_response
end

module Delete_room = struct
  type query = Lobby.Room.Id.t [@@deriving bin_io, sexp]
  type response =
    ( unit
    , [ `No_such_room | `Room_in_use ]
    ) Result.t [@@deriving bin_io, sexp]

  let rpc =
    Rpc.Rpc.create ~name:"delete-room" ~version:1 ~bin_query ~bin_response
end

type not_in_a_room =
  [ not_logged_in
  | `Not_in_a_room
  ] [@@deriving bin_io, sexp]

module Start_playing = struct
  type query =
    | Sit_in of Lobby.Room.Seat.t
    | Sit_anywhere
    [@@deriving bin_io, sexp]
  type error =
    [ not_in_a_room
    | `You're_already_playing
    | `Seat_occupied
    | `Game_already_started
    ] [@@deriving bin_io, sexp]
  type response =
    ( Lobby.Room.Seat.t
    , error
    ) Result.t [@@deriving bin_io, sexp]

  let rpc =
    Rpc.Rpc.create
      ~name:"start-playing" ~version:1
      ~bin_query ~bin_response
end

module Is_ready = struct
  type query = bool [@@deriving bin_io, sexp]
  type response =
    ( unit
    , [ not_in_a_room
      | `You're_not_playing
      | `Game_already_started
      ]
    ) Result.t
    [@@deriving bin_io, sexp]
  let rpc = Rpc.Rpc.create ~name:"ready" ~version:1 ~bin_query ~bin_response
end

module Chat = struct
  type query = string [@@deriving bin_io, sexp]
  type response = (unit, [ not_logged_in | `Chat_disabled ]) Result.t
    [@@deriving bin_io, sexp]
  let rpc = Rpc.Rpc.create ~name:"chat" ~version:1 ~bin_query ~bin_response
end

type not_playing =
  [ not_in_a_room
  | `Game_not_in_progress
  | `You're_not_playing
  ] [@@deriving bin_io, sexp]

let playing_exn =
  function
  | Ok x -> x
  | Error not_playing ->
    Error.raise_s [%message
      "Protocol.playing_exn"
        (not_playing : not_playing)
    ]

module Time_remaining = struct
  type span = Time_ns.Span.t [@@deriving bin_io]
  let sexp_of_span = Time_ns.Span.Alternate_sexp.sexp_of_t
  let span_of_sexp = Time_ns.Span.Alternate_sexp.t_of_sexp

  type query = unit [@@deriving bin_io, sexp]
  type response = (span, not_playing) Result.t [@@deriving bin_io, sexp]
  let rpc =
    Rpc.Rpc.create ~name:"time-left" ~version:1 ~bin_query ~bin_response
end

(* The response comes via the updates stream rather than in the RPC response,
   so that it has guaranteed ordering wrt other updates. *)
module Get_update = struct
  type query =
    | Hand
    | Market
    [@@deriving bin_io, sexp]
  type response = (unit, not_playing) Result.t [@@deriving bin_io, sexp]
  let rpc =
    Rpc.Rpc.create ~name:"get-update" ~version:1 ~bin_query ~bin_response
end

module Order = struct
  type query = Market.Order.t [@@deriving bin_io, sexp]

  type bad_price =
    [ `Price_must_be_nonnegative
    | `Price_too_high
    ] [@@deriving bin_io, sexp]

  type error =
    [ not_playing
    | `Owner_is_not_sender
    | `Duplicate_order_id
    | bad_price
    | `Size_must_be_positive
    | `Not_enough_to_sell
    ] [@@deriving bin_io, sexp]
  type response = ([ `Ack ], error) Result.t [@@deriving bin_io, sexp]
  let rpc = Rpc.Rpc.create ~name:"order" ~version:1 ~bin_query ~bin_response
end

(* Note that when you get the ack back, the order is cancelled, but it's
   possible that fills on it that already happened are still in flight. You
   should wait for an Out to show up in the updates pipe before assuming
   nothing more can be done on the order. *)
module Cancel = struct
  type query = Market.Order.Id.t [@@deriving bin_io, sexp]
  type error =
    [ not_playing
    | `No_such_order
    ] [@@deriving bin_io, sexp]
  type response = ([ `Ack ], error) Result.t [@@deriving bin_io, sexp]
  let rpc = Rpc.Rpc.create ~name:"cancel" ~version:1 ~bin_query ~bin_response
end

module Cancel_all = struct
  type query = unit [@@deriving bin_io, sexp]
  type error = not_playing [@@deriving bin_io, sexp]
  type response = ([ `Ack ], error) Result.t [@@deriving bin_io, sexp]
  let rpc = Rpc.Rpc.create ~name:"cxl-all" ~version:1 ~bin_query ~bin_response
end
