open Core.Std
open Async.Std

module Username : Identifiable.S = String

module Update = struct
  type t =
    | Waiting_for of int
    | Dealt of int Card.Hand.t
    [@@deriving bin_io, sexp]
end

module Join_game = struct
  type query = Username.t [@@deriving bin_io]
  type error = [ `Game_is_full | `Game_already_started ] [@@deriving bin_io]
  type response = Update.t [@@deriving bin_io]
  let rpc =
    Rpc.Pipe_rpc.create
      ~name:"join" ~version:1 ~bin_query ~bin_response ~bin_error
      ()
end

module Is_ready = struct
  type query = bool [@@deriving bin_io]
  type response = (unit, [ `Already_playing ]) Result.t [@@deriving bin_io]
  let rpc = Rpc.Rpc.create ~name:"ready" ~version:1 ~bin_query ~bin_response
end
