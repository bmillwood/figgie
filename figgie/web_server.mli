open Async.Std

type t

val create : port:int -> t Deferred.t

val broadcast : t -> Protocol.Broadcast.t -> unit
val market    : t -> Market.Book.t -> unit
val hands     : t -> Market.Size.t Card.Hand.t Username.Map.t -> unit
