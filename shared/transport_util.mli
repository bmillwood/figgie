open Core_kernel.Std
open Async_kernel.Std
open Async_rpc_kernel.Std

type 'a on_message =
  Bigstring.t -> pos:int -> len:int -> 'a Rpc.Transport.Handler_result.t

val read_forever_of_chunk_pipe
  :  Bigstring.t Pipe.Reader.t
  -> unconsumed:Bigsubstring.t ref
  -> is_closed:(unit -> bool)
  -> on_message:'a on_message
  -> on_end_of_batch:(unit -> unit)
  -> ('a, [ `Eof | `Closed ]) Result.t Deferred.t
