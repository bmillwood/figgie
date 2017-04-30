open Core_kernel.Std

val room_size : int

module User : sig
  module Role : sig
    type t =
      | Player   of { score : Market.Price.t; hand : Partial_hand.t }
      | Observer of { is_omniscient : bool }
      [@@deriving bin_io, sexp]
  end

  type t =
    { username : Username.t
    ; role : Role.t
    ; is_connected : bool
    } [@@deriving bin_io, sexp]
end

module Room : sig
  module Id : Identifiable.S
  type t [@@deriving bin_io, sexp]

  val empty : t

  val users      : t -> User.t Username.Map.t
  val has_user   : t -> username:Username.t -> bool
  val is_full    : t -> bool
  val can_delete : t -> bool

  module Update : sig
    type room

    module User_event : sig
      type t =
        | Joined
        | Observer_became_omniscient
        | Observer_started_playing
        | Player_score of Market.Price.t
        | Player_hand  of Partial_hand.t
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