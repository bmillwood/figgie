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
  type t [@@deriving bin_io, sexp]

  val empty : t

  val users      : t -> User.t Username.Map.t
  val has_player : t -> username:Username.t -> bool
  val is_full    : t -> bool
  val can_delete : t -> bool

  module Update : sig
    type room

    module User_event : sig
      type t =
        | Joined
        | Disconnected
      [@@deriving bin_io, sexp]
    end

    type t = { username : Username.t; event : User_event.t }
    [@@deriving bin_io, sexp]

    val apply : t -> room -> room
  end with type room := t
end

type t =
  { rooms : Room.t Room.Id.Map.t
  ; others : int Username.Map.t
  } [@@deriving bin_io, sexp]

val empty : t

module Update : sig
  type lobby

  module Where : sig
    type t =
      | Lobby
      | In_room of Room.Id.t
  end

  type t =
    | New_empty_room of { room_id : Room.Id.t }
    | Room_closed    of { room_id : Room.Id.t }
    | User_update    of { where : Where.t; update : Room.Update.t }
    [@@deriving bin_io, sexp]

  val apply : t -> lobby -> lobby
end with type lobby := t
