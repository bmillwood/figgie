open Async_rpc_kernel
open Figgie

type t =
  | Never
  | My_trades
  | Any_trades

val initial : t

val exec
  :  t
  -> exec:Market.Exec.t
  -> my_name:Username.t
  -> conn:Rpc.Connection.t
  -> unit
