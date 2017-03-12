open Async

open Figgie

type 'a t =
  { config : 'a
  ; username : Username.t
  ; conn : Rpc.Connection.t
  ; updates : Protocol.Game_update.t Pipe.Reader.t
  ; new_order_id : unit -> Market.Order.Id.t
  }

val make_command
  :  summary:string
  -> config_param:'a Command.Param.t
  -> username_stem:string
  -> f:('a t -> unit Deferred.t)
  -> Command.t
