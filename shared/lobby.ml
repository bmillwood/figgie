open Core_kernel.Std

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
      module Phase = struct
        type t =
          | Waiting of { is_ready : bool }
          | Playing
          [@@deriving bin_io, sexp]
      end

      type t =
        { score : Market.Price.t
        ; hand  : Partial_hand.t
        ; phase : Phase.t
        }
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
end

module Room = struct
  module Id = struct
    include String
    let is_valid id =
      not (String.is_empty id) && String.for_all id ~f:Char.is_print
  end

  module Seat = struct
    module T = struct
      type t = | North | East | South | West
        [@@deriving bin_io, compare, enumerate, sexp]
    end
    include T
    include Comparable.Make_binable(T)
  end


  type t =
    { seating : Username.t Seat.Map.t
    ; users   : User.t Username.Map.t
    } [@@deriving bin_io, sexp]

  let empty =
    { seating = Seat.Map.empty
    ; users = Username.Map.empty
    }

  let users t = t.users

  let players t =
    Map.filter_map t.users ~f:(fun user ->
        match user.role with
        | Observer _ -> None
        | Player p -> Some { user with role = p }
      )

  let seating t = t.seating

  let in_seat t ~seat =
    let open Option.Let_syntax in
    let%bind username = Map.find t.seating seat in
    let%bind user = Map.find t.users username in
    match user.role with
    | Player role -> Some { user with role }
    | Observer _ -> None

  let has_user   t ~username = Map.mem (users t)   username
  let has_player t ~username = Map.mem (players t) username

  let is_full t = Map.length t.seating >= Params.num_players

  let is_empty t = Map.is_empty t.users

  let is_ready t =
    let players = players t in
    let enough_players =
      Int.equal Params.num_players (Map.length players)
    in
    let all_ready =
      Map.for_all players ~f:(fun p ->
        match p.role.phase with
        | Waiting { is_ready } -> is_ready
        | Playing -> true
      )
    in
    enough_players && all_ready

  let can_delete t =
    Map.for_all t.users ~f:(fun user -> not user.is_connected)

  module Update = struct
    module User_event = struct
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

    type t =
      | Start_waiting
      | Start_playing
      | Player_event of { username : Username.t; event : User_event.t }
    [@@deriving bin_io, sexp]

    let set_phases room ~phase =
      let users =
        Map.map (users room) ~f:(fun player ->
            let role : User.Role.t =
              match player.role with
              | Player p -> Player { p with phase }
              | Observer _ -> player.role
            in
            { player with role }
          )
      in
      { room with users }

    let apply_player_update room ~username ~(event : User_event.t) =
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
            | Observer_started_playing { in_seat = _ } ->
              set_role (Player
                  { score = Market.Price.zero
                  ; hand = Partial_hand.starting
                  ; phase = Waiting { is_ready = false }
                  }
                )
            | Player_score score ->
              begin match role with
              | Player p -> set_role (Player { p with score })
              | Observer _ -> unchanged
              end
            | Player_hand hand ->
              begin match role with
              | Player p -> set_role (Player { p with hand })
              | Observer _ -> unchanged
              end
            | Player_ready is_ready ->
              begin match role with
              | Player ({ phase = Waiting _; _ } as p) ->
                set_role (Player { p with phase = Waiting { is_ready } })
              | Player { phase = Playing; _ } | Observer _ -> unchanged
              end
            | Disconnected ->
              set role false
            end
        )
      in
      { users
      ; seating =
          match event with
          | Observer_started_playing { in_seat } ->
            Map.add room.seating ~key:in_seat ~data:username
          | _ -> room.seating
      }

    let apply update room =
      match update with
      | Start_waiting ->
        set_phases room ~phase:(Waiting { is_ready = false })
      | Start_playing ->
        set_phases room ~phase:Playing
      | Player_event { username; event } ->
        apply_player_update room ~username ~event
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
        match update with
        | Player_event { username; event = Joined } ->
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
