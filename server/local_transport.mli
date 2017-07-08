open! Core
open Async
open Async_rpc_kernel

type t

val create : unit -> t

val listen : t -> f:(Rpc.Transport.t -> unit Deferred.t) -> unit

val connect : t -> Rpc.Connection.t Deferred.Or_error.t
