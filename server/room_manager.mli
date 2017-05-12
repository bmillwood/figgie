open! Core
open Async

open Figgie

type t

val create
  :  game_config:Game.Config.t
  -> id:Lobby.Room.Id.t
  -> lobby_updates:Protocol.Lobby_update.t Updates_manager.t
  -> t

val room_snapshot : t -> Lobby.Room.t
val is_empty      : t -> bool

val player_join
  :  t
  -> username:Username.t
  -> Protocol.Game_update.t Pipe.Reader.t

val chat : t -> username:Username.t -> string -> unit

val player_disconnected : t -> username:Username.t -> unit

val start_playing
  :  t
  -> username:Username.t
  -> in_seat:Protocol.Start_playing.query
  -> Protocol.Start_playing.response

val rpc_implementations
  : (t * Username.t, Protocol.not_in_a_room) Result.t
    Rpc.Implementation.t list
