open! Core
open Async

open Figgie

val command : Command.t

val run
  :  conn:Rpc.Connection.t
  -> avoid_username:(Username.t -> bool)
  -> room_choice:Room_choice.t
  -> where_to_sit:Protocol.Start_playing.query
  -> unit Deferred.t
