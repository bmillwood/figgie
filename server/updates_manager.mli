open Async

open Figgie

type 'update t

val create : unit -> 'update t

val subscribe
  :  'update t
  -> username:Username.t
  -> updates:'update Pipe.Writer.t
  -> unit

val unsubscribe : _ t -> username:Username.t -> unit

val update    : 'update t -> username:Username.t -> 'update      -> unit
val updates   : 'update t -> username:Username.t -> 'update list -> unit
val broadcast : 'update t ->                        'update      -> unit
