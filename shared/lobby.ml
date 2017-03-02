open Core_kernel.Std

let room_size = 4

module Room = struct
  module Id : Identifiable.S = String
  type t = {
    players : Username.t list;
  } [@@deriving bin_io, sexp]

  let empty = { players = [] }

  let has_player t ~username =
    List.mem t.players username ~equal:Username.equal

  let add_player t ~username =
    { players = username :: t.players }

  let is_full t = List.length t.players >= room_size
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
  type t =
    | Snapshot of T.t
    | New_room of { id : Room.Id.t; room : Room.t }
    | Room_closed of Room.Id.t
    | Player_joined_room of { player : Username.t; room_id : Room.Id.t }
    | Other_login of Username.t
    | Other_logout of Username.t
    [@@deriving bin_io, sexp]

  let apply t lobby =
    match t with
    | Snapshot snap -> snap
    | New_room { id; room } ->
      { lobby with rooms = Map.add lobby.rooms ~key:id ~data:room }
    | Room_closed id ->
      { lobby with rooms = Map.remove lobby.rooms id }
    | Player_joined_room { player; room_id } ->
      { rooms =
          Map.update lobby.rooms room_id ~f:(fun room ->
            let room = Option.value room ~default:Room.empty in
            Room.add_player room ~username:player
          )
      ; others = decr_count_map lobby.others player
      }
    | Other_login username ->
      { lobby with others =
          Map.update lobby.others username ~f:(fun maybe_count ->
            1 + Option.value maybe_count ~default:0)
      }
    | Other_logout username ->
      { lobby with others = decr_count_map lobby.others username }
end
