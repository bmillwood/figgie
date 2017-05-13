open Core

open Figgie
open Market

module Config : sig
  type t
  val arg : t Command.Param.t
end

module Waiting : sig
  type t
end

module Round : sig
  type t

  val end_time : t -> Time_ns.t

  val market : t -> Book.t

  val get_hand
    :  t
    -> username:Username.t
    -> (Size.t Card.Hand.t, [ `You're_not_playing ]) Result.t

  val add_order
    :  t
    -> order:Order.t
    -> sender:Username.t
    -> (Exec.t, Protocol.Order.error) Result.t

  val cancel_order
    :  t
    -> id:Order.Id.t
    -> sender:Username.t
    -> (Order.t, [ `No_such_order | `You're_not_playing ]) Result.t

  val cancel_orders
    :  t
    -> sender:Username.t
    -> (Order.t list, [ `You're_not_playing ]) Result.t
end

module Phase : sig
  type t =
    | Waiting_for_players of Waiting.t
    | Playing of Round.t
end

type t

val phase : t -> Phase.t

val create : config:Config.t -> t

val scores : t -> Price.t Username.Map.t

val player_join
  :  t
  -> username:Username.t 
  -> (unit, [> `Game_already_started | `Seat_occupied ]) Result.t

val set_ready
  :  t
  -> username:Username.t
  -> is_ready:bool
  -> ( [ `Started of Round.t | `Still_waiting ]
     , [ `You're_not_playing | `Game_already_in_progress ]
     ) Result.t

val end_round : t -> Round.t -> Protocol.Round_results.t
