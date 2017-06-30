open! Core
open Async
open Figgie

type t =
  | First_available
  | Named of Lobby.Room.Id.t

val param : t Command.Param.t

val join
  :  t
  -> conn:Rpc.Connection.t
  -> my_name:Username.t
  -> (Lobby.Room.Id.t * Protocol.Game_update.t Pipe.Reader.t) Deferred.t
