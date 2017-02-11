open Async.Std

type t = {
  username : Username.t;
  conn : Rpc.Connection.t;
  updates : Protocol.Game_update.t Pipe.Reader.t;
  new_order_id : unit -> Market.Order.Id.t;
}

val log_level_flag : Log.Level.t Command.Param.t

val make_command
  :  summary:string
  -> param:'a Command.Param.t
  -> username:('a -> Username.t)
  -> room_id:('a -> Lobby.Room.Id.t)
  -> f:(t -> 'a -> unit Deferred.t)
  -> Command.t
