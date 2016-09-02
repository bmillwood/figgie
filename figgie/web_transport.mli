open Core.Std
open Async.Std
module Rpc_kernel = Async_rpc_kernel.Std
module Rpc_transport = Rpc_kernel.Rpc.Transport

val serve
  :  port:int
  -> f:(Socket.Address.Inet.t -> Rpc_transport.t -> unit Deferred.t)
  -> (unit, unit) Result.t Deferred.t
