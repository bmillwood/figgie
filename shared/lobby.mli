open Core_kernel.Std

val room_size : int

module Room : sig
  module Id : Identifiable.S
  type t = { players : Username.t list } [@@deriving bin_io, sexp]

  val empty : t
  val has_player : t -> username:Username.t -> bool
  val add_player : t -> username:Username.t -> t
  val is_full : t -> bool
end

type t =
  { rooms : Room.t Room.Id.Map.t
  ; others : int Username.Map.t
  } [@@deriving bin_io, sexp]

val empty : t

module Update : sig
  type lobby
  type t =
    | Snapshot of lobby
    | New_room of { id : Room.Id.t; room : Room.t }
    | Room_closed of Room.Id.t
    | Player_joined_room of { player : Username.t; room_id : Room.Id.t }
    | Other_login of Username.t
    | Other_logout of Username.t
    [@@deriving bin_io, sexp]

  val apply : t -> lobby -> lobby
end with type lobby := t
