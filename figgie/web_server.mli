open Async.Std

type t

val create : port:int -> t Deferred.t

val broadcast : t -> Protocol.Broadcast.t -> unit
val market    : t -> Market.Book.t -> unit
