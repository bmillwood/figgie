open Core_kernel.Std

module User = struct
  module Gen = struct
    type 'role t =
      { username     : Username.t
      ; role         : 'role
      ; is_connected : bool
      ; is_bot       : bool
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
    ; gold    : Card.Suit.t option
    } [@@deriving bin_io, sexp]

  let empty =
    { seating = Seat.Map.empty
    ; users = Username.Map.empty
    ; gold = None
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
    Map.for_all t.users ~f:(fun user ->
        not user.is_connected || user.is_bot
      )

  module Update = struct
    module User_event = struct
      type t =
        | Joined of { is_bot : bool }
        | Observer_became_omniscient
        | Observer_started_playing of { in_seat : Seat.t }
        | Player_ready of bool
        | Disconnected
        [@@deriving bin_io, sexp]
    end

    module Round_results = struct
      type t =
        { gold : Card.Suit.t
        ; hands : Market.Size.t Card.Hand.t Username.Map.t
        } [@@deriving bin_io, sexp]

      let positions_this_round t =
        let winners, _, losers =
          Map.fold t.hands
            ~init:(Username.Map.empty, Market.Size.zero, Username.Map.empty)
            ~f:(fun ~key:username ~data:hand (winners, winning_amount, losers) ->
                let gold = Card.Hand.get hand ~suit:t.gold in
                match Ordering.of_int (Market.Size.compare gold winning_amount) with
                | Greater ->
                  ( Username.Map.singleton username hand
                  , gold
                  , Map.merge winners losers ~f:(fun ~key:_ -> function
                        | `Left x | `Right x -> Some x
                        | `Both (_, _) -> assert false)
                  )
                | Equal ->
                  ( Map.add winners ~key:username ~data:hand
                  , gold
                  , losers
                  )
                | Less ->
                  ( winners
                  , winning_amount
                  , Map.add losers ~key:username ~data:hand
                  )
              )
        in
        let total_gold_cards =
          let open Market.Size.O in
          let sum_map =
            Map.fold ~init:zero ~f:(fun ~key:_ ~data:hand acc ->
                acc + Card.Hand.get hand ~suit:t.gold
              )
          in
          sum_map winners + sum_map losers
        in
        let pot_size =
          Market.O.(Price.(
              Params.pot - total_gold_cards *$ Params.gold_card_value
            ))
        in
        let pot_per_winner =
          Market.Price.O.(pot_size / Map.length winners)
        in
        Map.merge winners losers
          ~f:(fun ~key:_ merge ->
              let is_winner, hand =
                match merge with
                | `Left hand -> true, hand
                | `Right hand -> false, hand
                | `Both (_, _) -> assert false
              in
              let num_gold_cards = Card.Hand.get hand ~suit:t.gold in
              let score_from_gold_cards =
                Market.O.(num_gold_cards *$ Params.gold_card_value)
              in
              let score_from_pot =
                if is_winner then pot_per_winner else Market.Price.zero
              in
              let cash =
                Market.Price.O.(score_from_gold_cards + score_from_pot)
              in
              Some { Market.Positions.cash; stuff = hand }
            )
    end

    type t =
      | Start_round
      | Player_event of { username : Username.t; event : User_event.t }
      | Exec of Market.Exec.t
      | Round_over of Round_results.t
    [@@deriving bin_io, sexp]

    let apply_player_update room ~username ~(event : User_event.t) =
      let users =
        Map.change (users room) username ~f:(
          function
          | None ->
            begin match event with
            | Joined { is_bot } ->
              Some
                { username
                ; role = Observer { is_omniscient = false }
                ; is_connected = true
                ; is_bot
                }
            | _ -> None
            end
          | Some { username; role; is_connected; is_bot } as unchanged ->
            let set (role : User.Role.t) is_connected is_bot =
              Some { User.Gen.username; role; is_connected; is_bot }
            in
            let set_role role = set role is_connected is_bot in
            begin match event with
            | Joined { is_bot } ->
              set role true is_bot
            | Observer_became_omniscient ->
              set_role (Observer { is_omniscient = true })
            | Observer_started_playing { in_seat = _ } ->
              set_role (Player
                  { score = Market.Price.zero
                  ; hand = Partial_hand.empty
                  ; phase = Waiting { is_ready = false }
                  }
                )
            | Player_ready is_ready ->
              begin match role with
              | Player ({ phase = Waiting _; _ } as p) ->
                set_role (Player { p with phase = Waiting { is_ready } })
              | Player { phase = Playing; _ } | Observer _ -> unchanged
              end
            | Disconnected ->
              set role false is_bot
            end
        )
      in
      let seating =
        match event with
        | Observer_started_playing { in_seat } ->
          Map.add room.seating ~key:in_seat ~data:username
        | _ -> room.seating
      in
      { room with users; seating }

    let apply update room =
      match update with
      | Start_round ->
        let users =
          Map.map (users room) ~f:(fun player ->
              let role : User.Role.t =
                match player.role with
                | Player p ->
                  let hand = Partial_hand.starting in
                  let score =
                    Market.Price.O.(p.score - Params.pot_per_player)
                  in
                  Player { phase = Playing; hand; score }
                | Observer _ -> player.role
              in
              { player with role }
            )
        in
        { room with users; gold = None }
      | Player_event { username; event } ->
        apply_player_update room ~username ~event
      | Exec exec ->
        let adjust_for_posted_sell =
          let do_nothing _username hand = hand in
          match exec.posted with
          | None -> do_nothing
          | Some posted ->
            Market.Dir.fold posted.dir
              ~buy:do_nothing
              ~sell:(fun username hand ->
                  if Username.equal posted.owner username then (
                    Partial_hand.selling hand
                      ~suit:posted.symbol ~size:posted.size
                  ) else (
                    hand
                  )
                )
        in
        let users =
          Map.merge
            (users room)
            (Market.Exec.position_effect exec)
            ~f:(fun ~key:_ ->
                function
                | `Left user ->
                  Some (user, Market.Positions.zero)
                | `Both (user, pos) ->
                  Some (user, pos)
                | `Right _pos ->
                  None
              )
          |> Map.map ~f:(fun ((user : User.t), (pos : Market.Positions.t)) ->
              match user.role with
              | Observer _ -> user
              | Player p ->
                let score = Market.Price.O.(p.score + pos.cash) in
                let hand =
                  Partial_hand.apply_positions_diff
                    p.hand
                    ~diff:pos.stuff
                  |> adjust_for_posted_sell user.username
                in
                { user with role = Player { p with score; hand } }
            )
        in
        { room with users }
      | Round_over results ->
        let users =
          Map.merge
            (users room)
            (Round_results.positions_this_round results)
            ~f:(fun ~key:_ ->
              function
              | `Left user -> Some user
              | `Right _ -> None
              | `Both (user, pos) ->
                let role : User.Role.t =
                  match user.role with
                  | Player p ->
                    let phase : User.Player.Data.Phase.t =
                      Waiting { is_ready = false }
                    in
                    let hand = Partial_hand.create_known pos.stuff in
                    let score = Market.Price.O.(p.score + pos.cash) in
                    Player { hand; score; phase }
                  | Observer _ -> user.role
                in
                Some { user with role }
            )
        in
        { room with users; gold = Some results.gold }
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
        | Player_event { username; event = Joined { is_bot = false } } ->
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
