open Async_rpc_kernel.Std
open Core_kernel.Std
open Async_kernel.Std

val connect
  :  Host_and_port.t
  -> (Rpc.Transport.t, unit) Result.t Deferred.t
