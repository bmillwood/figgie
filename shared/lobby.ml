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

  let has_player t ~username = Map.mem t.users username

  let set_player t ~username ~is_connected =
    { users = Map.add t.users ~key:username ~data:{ username; is_connected } }

  let is_full t = Map.length t.users >= room_size
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
  module Player_event = struct
    type t =
      | Joined
      | Disconnected
      [@@deriving bin_io, sexp]
  end

  type t =
    | Snapshot of T.t
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

  let apply t lobby =
    match t with
    | Snapshot snap -> snap
    | New_room { id; room } ->
      { lobby with rooms = Map.add lobby.rooms ~key:id ~data:room }
    | Room_closed id ->
      { lobby with rooms = Map.remove lobby.rooms id }
    | Player_event { username; room_id; event } ->
      let is_connected, others =
        match event with
        | Joined       -> true,  decr_count_map lobby.others username
        | Disconnected -> false, lobby.others
      in
      let rooms =
        Map.update lobby.rooms room_id ~f:(fun room ->
          let room = Option.value room ~default:Room.empty in
          Room.set_player room ~username ~is_connected
        )
      in
      { rooms; others }
    | Other_login username ->
      { lobby with others =
          Map.update lobby.others username ~f:(fun maybe_count ->
            1 + Option.value maybe_count ~default:0)
      }
    | Other_disconnect username ->
      { lobby with others = decr_count_map lobby.others username }
end
