open Core_kernel.Std

let room_size = 4

module User = struct
  module T = struct
    type t =
      { username : Username.t
      ; is_connected : bool
      } [@@deriving bin_io, compare, sexp]
  end
  include T
  include Comparable.Make_binable(T)
end

module Room = struct
  module Id : Identifiable.S = String
  type t = {
    users : User.t Username.Map.t;
  } [@@deriving bin_io, sexp]

  let empty = { users = Username.Map.empty }

  let users t = t.users

  let has_player t ~username = Map.mem t.users username

  let is_full t = Map.length t.users >= room_size

  let can_delete t =
    Map.for_all t.users ~f:(fun user -> not user.is_connected)

  module Update = struct
    module User_event = struct
      type t =
        | Joined
        | Disconnected
        [@@deriving bin_io, sexp]
    end

    type t = { username : Username.t; event : User_event.t }
    [@@deriving bin_io, sexp]

    let apply { username; event } room =
      let is_connected =
        match event with
        | Joined -> true
        | Disconnected -> false
      in
      let player = { User.username; is_connected } in
      { users = Map.add room.users ~key:username ~data:player }
  end
end

module T = struct
  type t =
    { rooms : Room.t Room.Id.Map.t
    ; others : int Username.Map.t
    }
    [@@deriving bin_io, sexp]
end
include T

let decr_count_map t key =
  Map.change t key ~f:(fun maybe_count ->
    match maybe_count with
    | None -> None
    | Some c when c <= 1 -> None
    | Some c -> Some (c - 1))

let empty = { rooms = Room.Id.Map.empty; others = Username.Map.empty }

module Update = struct
  module Where = struct
    type t =
      | Lobby
      | In_room of Room.Id.t
    [@@deriving bin_io, sexp]
  end

  type t =
    | New_empty_room of { room_id : Room.Id.t }
    | Room_closed    of { room_id : Room.Id.t }
    | User_update    of { where : Where.t; update : Room.Update.t }
    [@@deriving bin_io, sexp]

  let apply t lobby =
    match t with
    | New_empty_room { room_id } ->
      let rooms = Map.add lobby.rooms ~key:room_id ~data:Room.empty in
      { lobby with rooms }
    | Room_closed { room_id } ->
      { lobby with rooms = Map.remove lobby.rooms room_id }
    | User_update { where = In_room room_id; update } ->
      let rooms =
        Map.update lobby.rooms room_id ~f:(fun room ->
          let room = Option.value room ~default:Room.empty in
          Room.Update.apply update room
        )
      in
      let others =
        let { Room.Update. username; event } = update in
        match event with
        | Joined       -> decr_count_map lobby.others username
        | Disconnected -> lobby.others
      in
      { rooms; others }
    | User_update { where = Lobby; update = { username; event } } ->
      let others =
        match event with
        | Joined ->
          Map.update lobby.others username ~f:(fun maybe_count ->
            1 + Option.value maybe_count ~default:0)
        | Disconnected ->
          decr_count_map lobby.others username
      in
      { lobby with others }
end
