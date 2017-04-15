open Core_kernel.Std

val room_size : int

module User : sig
  type t =
    { username : Username.t
    ; is_connected : bool
    } [@@deriving bin_io, sexp]

  include Comparable.S_binable with type t := t
end

module Room : sig
  module Id : Identifiable.S
  type t = { users : User.t Username.Map.t } [@@deriving bin_io, sexp]

  val empty : t
  val has_player : t -> username:Username.t -> bool
  val set_player : t -> username:Username.t -> is_connected:bool -> t
  val is_full : t -> bool
end

type t =
  { rooms : Room.t Room.Id.Map.t
  ; others : int Username.Map.t
  } [@@deriving bin_io, sexp]

val empty : t

module Update : sig
  type lobby

  module Player_event : sig
    type t =
      | Joined
      | Disconnected
      [@@deriving bin_io, sexp]
  end

  type t =
    | Snapshot of lobby
    | New_room of { id : Room.Id.t; room : Room.t }
    | Room_closed of Room.Id.t
    | Player_event of
      { username : Username.t
      ; room_id : Room.Id.t
      ; event : Player_event.t
      }
    | Other_login of Username.t
    | Other_disconnect of Username.t
    [@@deriving bin_io, sexp]

  val apply : t -> lobby -> lobby
end with type lobby := t
