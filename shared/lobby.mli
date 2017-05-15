open Core_kernel.Std

val max_players_per_room : int

module User : sig
  module Gen : sig
    type 'role t =
      { username     : Username.t
      ; role         : 'role
      ; is_connected : bool
      }
  end

  module Player : sig
    module Data : sig
      type t =
        { score : Market.Price.t
        ; hand : Partial_hand.t
        ; is_ready : bool
        }
    end

    type t = Data.t Gen.t [@@deriving sexp]
  end

  module Observer : sig
    module Data : sig
      type t = { is_omniscient : bool }
    end

    type t = Data.t Gen.t [@@deriving sexp]
  end

  module Role : sig
    type t =
      | Player   of Player.Data.t
      | Observer of Observer.Data.t
  end

  type t = Role.t Gen.t [@@deriving sexp]

  val username     : t -> Username.t
  val role         : t -> Role.t
  val is_connected : t -> bool
end

module Room : sig
  module Id : Identifiable.S

  module Seat : sig
    type t [@@deriving bin_io, compare, enumerate, sexp]
    include Comparable.S_binable with type t := t
  end

  type t [@@deriving bin_io, sexp]

  val empty : t

  val users      : t -> User.t Username.Map.t
  val players    : t -> User.Player.t Username.Map.t
  val seating    : t -> Username.t Seat.Map.t
  val in_seat    : t -> seat:Seat.t -> User.Player.t option
  val has_user   : t -> username:Username.t -> bool
  val is_full    : t -> bool
  val is_empty   : t -> bool
  val can_delete : t -> bool

  module Update : sig
    type room

    module User_event : sig
      type t =
        | Joined
        | Observer_became_omniscient
        | Observer_started_playing of { in_seat : Seat.t }
        | Player_score of Market.Price.t
        | Player_hand  of Partial_hand.t
        | Player_ready of bool
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

  module User_event : sig
    type t =
      | Connected
      | Disconnected
    [@@deriving bin_io, sexp]
  end

  type t =
    | Lobby_update   of { username : Username.t; event : User_event.t }
    | New_empty_room of { room_id : Room.Id.t }
    | Room_closed    of { room_id : Room.Id.t }
    | Room_update    of { room_id : Room.Id.t; update : Room.Update.t }
    [@@deriving bin_io, sexp]

  val apply : t -> lobby -> lobby
end with type lobby := t
