open Core.Std

module Player = struct
  type t = {
    mutable username : Username.t;
    mutable chips : Market.Price.t;
  }

  let create ~username = { username; chips = Market.Price.zero }
end

module Waiting = struct
  module Player = struct
    type t = {
      p : Player.t;
      mutable is_ready : bool;
    }

    let create player = { p = player; is_ready = false }
  end

  type t = {
    players : Player.t Username.Table.t;
  }

  let waiting_for t =
    let waiting_for_connects =
      Int.max 0 (Params.min_players - Hashtbl.length t.players)
    in
    let waiting_for_readies = Hashtbl.count t.players ~f:(fun p -> not p.is_ready) in
    waiting_for_connects + waiting_for_readies
end

module Round = struct
  module Player = struct
    type t = {
      p : Player.t;
      mutable hand : Market.Size.t Card.Hand.t;
      orders : Market.Order.t Market.Order.Id.Table.t;
    }

    let create p =
      { p
      ; hand  = Card.Hand.create_all Market.Size.zero
      ; orders  = Market.Order.Id.Table.create ()
      }

    let sellable_hand t =
      Hashtbl.fold t.orders ~init:t.hand ~f:(fun ~key:_ ~data:order acc ->
        Card.Hand.modify acc ~suit:order.symbol
          ~f:(fun s -> Market.Size.O.(s - order.size)))
  end

  type t = {
    players : Player.t Username.Map.t;
    pot : Market.Price.t;
    mutable market : Market.Book.t;
    gold : Card.Suit.t;
  }

  let results t =
    let winners, _, losers =
      Map.fold t.players
        ~init:(Username.Map.empty, Market.Size.zero, Username.Map.empty)
        ~f:(fun ~key:username ~data:player (winners, winning_amount, losers) ->
          let gold = Card.Hand.get player.hand ~suit:t.gold in
          match Ordering.of_int (Market.Size.compare gold winning_amount) with
          | Greater ->
              ( Username.Map.singleton username gold
              , gold
              , Map.merge winners losers ~f:(fun ~key:_ -> function
                  | `Left x | `Right x -> Some x
                  | `Both (_, _) -> assert false)
              )
          | Equal -> (Map.add winners ~key:username ~data:gold, gold, losers)
          | Less -> (winners, winning_amount, Map.add losers ~key:username ~data:gold))
    in
    let total_gold_cards =
      let open Market.Size.O in
      let sum_map = Map.fold ~init:zero ~f:(fun ~key:_ ~data:n acc -> n + acc) in
      sum_map winners + sum_map losers
    in
    let pot_size =
      Market.O.(Price.(t.pot - total_gold_cards *$ Params.gold_card_value))
    in
    let pot_per_winner =
      Market.Price.O.(pot_size / Map.length winners)
    in
    let scores_this_round =
      Map.merge winners losers
        ~f:(fun ~key:_ -> function
          | `Left x -> Some Market.O.(Price.(x *$ Params.gold_card_value + pot_per_winner))
          | `Right x -> Some Market.O.(x *$ Params.gold_card_value)
          | `Both (_, _) -> assert false)
    in
    let hands = Map.map t.players ~f:(fun player -> player.hand) in
    { Protocol.Round_results.gold = t.gold; hands; scores_this_round }

  let start ~players =
    let num_players = Map.length players in
    assert (Market.Size.to_int Params.num_cards_in_deck mod num_players = 0);
    let hands, gold =
      let open Card in
      let long, short = Suit.random_two () in
      let deck = Hand.init ~f:(fun _ -> ref Params.normal_suit_cards) in
      Hand.get deck ~suit:long := Params.long_suit_cards;
      Hand.get deck ~suit:short := Params.short_suit_cards;
      let remaining_suits = ref Suit.all in
      let hands =
        Array.init num_players ~f:(fun _i ->
          Hand.init ~f:(fun _ -> ref Market.Size.zero))
      in
      for ix = 0 to Market.Size.to_int Params.num_cards_in_deck - 1 do
        let suit =
          List.nth_exn !remaining_suits
            (Random.int (List.length !remaining_suits))
        in
        let deck_suit = Hand.get deck ~suit in
        let hand_suit = Hand.get hands.(ix mod num_players) ~suit in
        deck_suit := Market.Size.(!deck_suit - of_int 1);
        hand_suit := Market.Size.(!hand_suit + of_int 1);
        begin if Market.Size.O.(!deck_suit = zero)
        then remaining_suits :=
          List.filter !remaining_suits ~f:(Fn.non (Suit.equal suit))
        end
      done;
      Array.map hands ~f:(Hand.map ~f:(fun r -> !r)), Suit.opposite long
    in
    let to_pot = Params.pot_per_player ~num_players in
    let pot = ref Market.Price.zero in
    let players =
      List.map2_exn (Map.data players) (Array.to_list hands) ~f:(fun p hand ->
        let player = Player.create p in
        player.hand <- hand;
        p.chips <- Market.Price.O.(p.chips - to_pot);
        pot := Market.Price.O.(!pot + to_pot);
        p.username, player)
      |> Username.Map.of_alist_exn
    in
    { players; pot = !pot; market = Market.Book.empty; gold }

  let add_order t ~order:(sent_order : Market.Order.t) ~(sender : Player.t) =
    let open Result.Monad_infix in
    Result.ok_if_true (Username.equal sent_order.owner sender.p.username)
      ~error:`Owner_is_not_sender
    >>= fun () ->
    Result.ok_if_true (not (Hashtbl.mem sender.orders sent_order.id))
      ~error:`Duplicate_order_id
    >>= fun () ->
    Result.ok_if_true Market.Price.O.(sent_order.price >= zero)
      ~error:`Price_must_be_nonnegative
    >>= fun () ->
    Result.ok_if_true Market.Size.O.(sent_order.size > zero)
      ~error:`Size_must_be_positive
    >>= fun () ->
    Result.ok_if_true
      (Market.Dir.equal sent_order.dir Buy ||
        let max_sell =
          Card.Hand.get (Player.sellable_hand sender) ~suit:sent_order.symbol
        in
        Market.Size.O.(sent_order.size <= max_sell))
      ~error:`Not_enough_to_sell
    >>| fun () ->
    let { Market.Match_result.exec; remaining } =
      Market.Book.match_ t.market sent_order
    in
    t.market <- remaining;
    let settle_order ~owner (executed_order : Market.Order.t) =
      let add_cards ~(player : Player.t) delta =
        player.hand <-
          Card.Hand.modify player.hand ~suit:executed_order.symbol
            ~f:(fun s -> Market.Size.O.(s + delta))
      in
      let size_for_owner =
        Market.Dir.fold executed_order.dir
          ~buy:Fn.id ~sell:Market.Size.neg
          executed_order.size
      in
      let cashflow_for_owner =
        Market.Dir.fold executed_order.dir
          ~buy:Market.Price.neg ~sell:Fn.id
          Market.O.(executed_order.size *$ executed_order.price)
      in
      add_cards ~player:owner  size_for_owner;
      add_cards ~player:sender (Market.Size.neg size_for_owner);
      owner.p.chips  <- Market.Price.O.(owner.p.chips  + cashflow_for_owner);
      sender.p.chips <- Market.Price.O.(sender.p.chips - cashflow_for_owner)
    in
    List.iter exec.fully_filled ~f:(fun executed_order ->
      let owner = Map.find_exn t.players executed_order.owner in
      settle_order ~owner executed_order;
      Hashtbl.remove owner.orders executed_order.id);
    Option.iter exec.partially_filled ~f:(fun partial_fill ->
      let executed_order = partial_fill.original_order in
      let owner = Map.find_exn t.players executed_order.owner in
      settle_order ~owner { executed_order with size = partial_fill.filled_by };
      let remaining_size =
        Market.Size.O.(executed_order.size - partial_fill.filled_by)
      in
      Hashtbl.set owner.orders
        ~key:executed_order.id
        ~data:{ executed_order with size = remaining_size });
    Option.iter exec.posted ~f:(fun order ->
      Hashtbl.add_exn sender.orders ~key:order.id ~data:order);
    exec

  let cancel t ~(id : Market.Order.Id.t) ~(sender : Player.t) =
    match Hashtbl.find sender.orders id with
    | None -> Error `No_such_order
    | Some order ->
      Result.map (Market.Book.cancel t.market order) ~f:(fun new_market ->
        t.market <- new_market;
        Hashtbl.remove sender.orders id;
        order)
end

module Phase = struct
  type t =
    | Waiting_for_players of Waiting.t
    | Playing of Round.t
end

type t = { mutable phase : Phase.t }

let create () = { phase = Waiting_for_players { players = Username.Table.create () } }

let waiting_for t =
  match t.phase with
  | Playing _ -> 0
  | Waiting_for_players waiting -> Waiting.waiting_for waiting

let num_players t =
  match t.phase with
  | Waiting_for_players waiting -> Hashtbl.length waiting.players
  | Playing round -> Map.length round.players

let player_join t ~username =
  match t.phase with
  | Playing _ -> Error `Game_already_started
  | Waiting_for_players waiting ->
    if Hashtbl.length waiting.players >= Params.max_players
    then Error `Game_is_full
    else begin
      let player =
        Hashtbl.find_or_add waiting.players username
          ~default:(fun () -> Waiting.Player.create (Player.create ~username))
      in
      ignore player;
      Ok ()
    end

let end_round t (round : Round.t) =
  let players = Username.Table.create () in
  Map.iteri round.players ~f:(fun ~key:_ ~data:player ->
    Hashtbl.set players ~key:player.p.username
      ~data:(Waiting.Player.create player.p));
  t.phase <- Waiting_for_players { players };
  let results = Round.results round in
  Map.iteri results.scores_this_round ~f:(fun ~key:player ~data:score ->
    let player = Hashtbl.find_exn players player in
    player.p.chips <- Market.Price.O.(player.p.chips + score));
  results

let set_ready t ~username ~is_ready =
  match t.phase with
  | Playing _ -> Error `Already_playing
  | Waiting_for_players waiting ->
      let player = Hashtbl.find_exn waiting.players username in
      player.is_ready <- is_ready;
      if is_ready && Waiting.waiting_for waiting = 0
      then begin
        let round_players =
          Hashtbl.to_alist waiting.players
          |> Username.Map.of_alist_exn
          |> Map.map ~f:(fun player -> player.p)
        in
        let round = Round.start ~players:round_players in
        t.phase <- Playing round;
        Ok (`Started round)
      end else Ok `Still_waiting

let with_player_in_game t ~username ~f =
  match t.phase with
  | Waiting_for_players _ -> Error `Game_not_in_progress
  | Playing round ->
    match Map.find round.players username with
    | None -> Error `You're_not_playing
    | Some player -> f ~round ~player

let add_order t ~order ~sender =
  with_player_in_game t ~username:sender ~f:(fun ~round ~player ->
    Round.add_order round ~order ~sender:player)

let cancel t ~id ~sender =
  with_player_in_game t ~username:sender ~f:(fun ~round ~player ->
    Round.cancel round ~id ~sender:player)

let get_hand t ~username =
  with_player_in_game t ~username ~f:(fun ~round:_ ~player ->
    Ok (player.hand, player.p.chips))
