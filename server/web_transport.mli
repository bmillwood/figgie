open Core
open Async
open Async_rpc_kernel

val serve
  :  port:int
  -> f:(Socket.Address.Inet.t -> Rpc.Transport.t -> unit Deferred.t)
  -> (unit, unit) Result.t Deferred.t
