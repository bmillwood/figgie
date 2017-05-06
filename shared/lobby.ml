open Core_kernel.Std

let room_size = 4

module User = struct
  module Gen = struct
    type 'role t =
      { username     : Username.t
      ; role         : 'role
      ; is_connected : bool
      } [@@deriving bin_io, sexp]
  end

  module Player = struct
    module Data = struct
      type t = { score : Market.Price.t; hand : Partial_hand.t }
        [@@deriving bin_io, sexp]
    end

    type t = Data.t Gen.t
      [@@deriving bin_io, sexp]
  end

  module Observer = struct
    module Data = struct
      type t = { is_omniscient : bool }
        [@@deriving bin_io, sexp]
    end

    type t = Data.t Gen.t
      [@@deriving bin_io, sexp]
  end

  module Role = struct
    type t =
      | Player   of Player.Data.t
      | Observer of Observer.Data.t
      [@@deriving bin_io, sexp]
  end

  type t = Role.t Gen.t
    [@@deriving bin_io, sexp]

  let username     (t : t) = t.username
  let role         (t : t) = t.role
  let is_connected (t : t) = t.is_connected

  let set_hand_if_player (t : t) ~hand : t =
    match t.role with
    | Player { score; hand = _ } ->
      { t with role = Player { score; hand } }
    | Observer _ -> t
end

module Room = struct
  module Id : Identifiable.S = String
  type t = {
    users : User.t Username.Map.t;
  } [@@deriving bin_io, sexp]

  let empty = { users = Username.Map.empty }

  let users t = t.users

  let has_user t ~username = Map.mem t.users username

  let is_full t = Map.length t.users >= room_size

  let can_delete t =
    Map.for_all t.users ~f:(fun user -> not user.is_connected)

  module Update = struct
    module User_event = struct
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

    let apply { username; event } room =
      let users =
        Map.change (users room) username ~f:(
          function
          | None ->
            begin match event with
            | Joined ->
              Some
                { username
                ; role = Observer { is_omniscient = false }
                ; is_connected = true
                }
            | _ -> None
            end
          | Some { username = _; role; is_connected } as unchanged ->
            let set (role : User.Role.t) is_connected =
              Some { User.Gen.username; role; is_connected }
            in
            let set_role role = set role is_connected in
            begin match event with
            | Joined ->
              set role true
            | Observer_became_omniscient ->
              set_role (Observer { is_omniscient = true })
            | Observer_started_playing ->
              let score = Market.Price.zero in
              let hand = Partial_hand.empty in
              set_role (Player { score; hand })
            | Player_score score ->
              begin match role with
              | Player { score = _; hand } ->
                set_role (Player { score; hand })
              | Observer _ -> unchanged
              end
            | Player_hand hand ->
              begin match role with
              | Player { score; hand = _ } ->
                set_role (Player { score; hand })
              | Observer _ -> unchanged
              end
            | Disconnected ->
              set role false
            end
        )
      in
      { users }
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
  module User_event = struct
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

  let apply t lobby =
    match t with
    | New_empty_room { room_id } ->
      let rooms = Map.add lobby.rooms ~key:room_id ~data:Room.empty in
      { lobby with rooms }
    | Room_closed { room_id } ->
      { lobby with rooms = Map.remove lobby.rooms room_id }
    | Room_update { room_id; update } ->
      let rooms =
        Map.update lobby.rooms room_id ~f:(fun room ->
          let room = Option.value room ~default:Room.empty in
          Room.Update.apply update room
        )
      in
      let others =
        let { Room.Update. username; event } = update in
        match event with
        | Joined ->
          decr_count_map lobby.others username
        | _ -> lobby.others
      in
      { rooms; others }
    | Lobby_update { username; event } ->
      let others =
        match event with
        | Connected ->
          Map.update lobby.others username ~f:(fun maybe_count ->
            1 + Option.value maybe_count ~default:0)
        | Disconnected ->
          decr_count_map lobby.others username
      in
      { lobby with others }
end
