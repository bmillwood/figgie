open Async

open Figgie
open Market

type t

val username : t -> Username.t
val updates  : t -> Protocol.Game_update.t Pipe.Reader.t

val unacked_orders : t -> Order.t list

val open_orders : t -> Book.t

(** [hand_if_no_fills] and [hand_if_filled] return [None] if we haven't heard
    what our hand is yet. [sellable_hand], meanwhile, just says 0
    in that case. *)
val hand_if_no_fills : t -> Size.t           Card.Hand.t option
val hand_if_filled   : t -> Size.t Dirpair.t Card.Hand.t option
val sellable_hand    : t -> Size.t           Card.Hand.t

val room_with_my_hand : t -> Lobby.Room.t option

module Staged_order : sig
  type bot
  type t

  val create
    :  bot
    -> symbol:Symbol.t
    -> dir:Dir.t
    -> price:Price.t
    -> size:Size.t
    -> t

  val id : t -> Order.Id.t

  val send_exn : t -> bot -> Protocol.Order.response Deferred.t
end with type bot := t

val cancel : t -> Order.Id.t -> Protocol.Cancel.response Deferred.t

val request_update_exn : t -> Protocol.Get_update.query -> unit Deferred.t

val try_set_ready : t -> unit Deferred.t

val make_command
  :  summary:string
  -> config_param:'a Command.Param.t
  -> username_stem:string
  -> ?auto_ready:bool
  -> (t -> config:'a -> unit Deferred.t)
  -> Command.t
