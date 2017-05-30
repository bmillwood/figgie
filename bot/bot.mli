open Async

open Figgie
open Market

type t

val username : t -> Username.t
val updates  : t -> Protocol.Game_update.t Pipe.Reader.t

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
