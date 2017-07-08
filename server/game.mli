open Core

open Figgie
open Market

module Config : sig
  type t
  val arg : t Command.Param.t
end

type t

val create : config:Config.t -> room:Lobby.Room.t -> t

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

val results : t -> Lobby.Room.Update.Round_results.t
