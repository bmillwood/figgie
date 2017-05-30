open Async

open Figgie

type t

val username : t -> Username.t
val conn     : t -> Rpc.Connection.t
val updates  : t -> Protocol.Game_update.t Pipe.Reader.t

val new_order_id : t -> Market.Order.Id.t

val try_set_ready : t -> unit Deferred.t

val make_command
  :  summary:string
  -> config_param:'a Command.Param.t
  -> username_stem:string
  -> f:(t -> config:'a -> unit Deferred.t)
  -> Command.t
