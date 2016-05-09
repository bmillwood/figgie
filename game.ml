open Async.Std
open Core.Std

module Player = struct
  type t = {
    mutable is_ready : bool;
    mutable username : Username.t;
    mutable hand  : Market.Size.t Card.Hand.t;
    mutable chips : Market.Price.t;
    orders : Market.Order.t Market.Order.Id.Table.t;
  }

  let create ~username =
    { is_ready = false
    ; username
    ; hand  = Card.Hand.create_all Market.Size.zero
    ; chips = Market.Price.zero
    ; orders  = Market.Order.Id.Table.create ()
    }

  let sellable_hand t =
    Hashtbl.fold t.orders ~init:t.hand ~f:(fun ~key:_ ~data:order acc ->
      Card.Hand.modify acc ~suit:order.symbol
        ~f:(fun s -> Market.Size.O.(s - order.size)))
end

module Round = struct
  type t = {
    mutable market : Market.t;
    gold : Card.Suit.t;
  }
end

module Stage = struct
  type t =
    | Waiting_for_players
    | Playing of Round.t
end

type t = {
  mutable stage : Stage.t;
  players : Player.t Username.Table.t;
  mutable pot : Market.Price.t;
}

let create () =
  { stage   = Waiting_for_players
  ; players = Username.Table.create ()
  ; pot     = Market.Price.zero
  }

let num_players t = Hashtbl.length t.players

let player t ~username =
  Hashtbl.find_or_add t.players username
    ~default:(fun () -> Player.create ~username)

let set_ready t ~(player : Player.t) ~is_ready =
  match t.stage with
  | Playing _ -> Error `Already_playing
  | Waiting_for_players ->
      player.is_ready <- is_ready;
      if is_ready
          && num_players t >= Params.min_players
          && Hashtbl.for_all t.players ~f:(fun player -> player.is_ready)
      then Ok `All_ready
      else Ok `Still_waiting

let waiting_for t =
  let waiting_for_connects = Int.max 0 (Params.min_players - num_players t) in
  let waiting_for_readies = Hashtbl.count t.players ~f:(fun p -> not p.is_ready) in
  waiting_for_connects + waiting_for_readies

let score t =
  match t.stage with
  | Waiting_for_players -> Username.Map.empty
  | Playing round ->
    let winners, _, losers =
      Hashtbl.fold t.players
        ~init:(Username.Map.empty, Market.Size.zero, Username.Map.empty)
        ~f:(fun ~key:username ~data:player (winners, winning_amount, losers) ->
          let gold = Card.Hand.get player.hand ~suit:round.gold in
          match Ordering.of_int (Market.Size.compare gold winning_amount) with
          | Greater ->
              ( Username.Map.singleton username gold
              , gold
              , Map.merge winners losers ~f:(fun ~key:_ -> function
                  | `Left x | `Right x -> Some x
                  | `Both (_, _) -> assert false)
              )
          | Equal -> (Map.add winners ~key:username ~data:gold, gold, losers)
          | Less -> (winners, gold, Map.add losers ~key:username ~data:gold))
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
    Map.merge winners losers
      ~f:(fun ~key:_ -> function
        | `Left x -> Some Market.O.(Price.(x *$ Params.gold_card_value + pot_per_winner))
        | `Right x -> Some Market.O.(x *$ Params.gold_card_value)
        | `Both (_, _) -> assert false)

let end_round t =
  let scores = score t in
  Hashtbl.iteri t.players ~f:(fun ~key:_ ~data:player ->
    player.is_ready <- false;
    player.chips <-
      Market.Price.O.(player.chips + Map.find_exn scores player.username);
    Hashtbl.clear player.orders);
  t.stage <- Waiting_for_players;
  scores

let new_round t =
  let hands, gold =
    let num_players = num_players t in
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
        List.nth_exn !remaining_suits (Random.int (List.length !remaining_suits))
      in
      let deck_suit = Hand.get deck ~suit in
      let hand_suit = Hand.get hands.(ix mod num_players) ~suit in
      deck_suit := Market.Size.(!deck_suit - of_int 1);
      hand_suit := Market.Size.(!hand_suit + of_int 1);
      if Market.Size.O.(!deck_suit = zero)
      then remaining_suits := List.filter !remaining_suits ~f:(Fn.non (Suit.equal suit))
    done;
    Array.map hands ~f:(Hand.map ~f:(fun r -> !r)), Suit.opposite long
  in
  let to_pot = Params.pot_per_player ~num_players in
  List.iter2_exn (Hashtbl.data t.players) (Array.to_list hands) ~f:(fun player hand ->
    player.hand <- hand;
    player.chips <- Market.Price.O.(player.chips - to_pot);
    t.pot <- Market.Price.O.(t.pot + to_pot));
  let round : Round.t = { market = Market.empty; gold } in
  t.stage <- Playing round;
  Clock.after Params.length_of_round
  >>| fun () ->
  end_round t

let add_order t ~order:(sent_order : Market.Order.t) ~(sender : Player.t) =
  let open Result.Monad_infix in
  let open Protocol.Reject in
  begin match t.stage with
  | Waiting_for_players -> Error Game_not_in_progress
  | Playing round -> Ok round
  end
  >>= fun round ->
  Result.ok_if_true (Username.equal sent_order.owner sender.username)
    ~error:Owner_is_not_sender
  >>= fun () ->
  Result.ok_if_true (not (Hashtbl.mem sender.orders sent_order.id))
    ~error:Duplicate_order_id
  >>= fun () ->
  Result.ok_if_true
    (Market.Dir.equal sent_order.dir Buy ||
      let max_sell =
        Card.Hand.get (Player.sellable_hand sender) ~suit:sent_order.symbol
      in
      Market.Size.O.(sent_order.size <= max_sell))
    ~error:Not_enough_to_sell
  >>| fun () ->
  let { Market.Match_result.exec; remaining } = Market.match_ round.market sent_order in
  round.market <- remaining;
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
    let price_for_owner =
      Market.Dir.fold executed_order.dir
        ~buy:Market.Price.neg ~sell:Fn.id
        Market.O.(executed_order.size *$ executed_order.price)
    in
    add_cards ~player:owner  size_for_owner;
    add_cards ~player:sender (Market.Size.neg size_for_owner);
    owner.chips <- Market.Price.O.(owner.chips + price_for_owner);
    owner.chips <- Market.Price.O.(owner.chips - price_for_owner)
  in
  List.iter exec.fully_filled ~f:(fun executed_order ->
    let owner = Hashtbl.find_exn t.players executed_order.owner in
    settle_order ~owner executed_order;
    Hashtbl.remove owner.orders executed_order.id);
  Option.iter exec.partially_filled ~f:(fun partial_fill ->
    let executed_order = partial_fill.original_order in
    let owner = Hashtbl.find_exn t.players executed_order.owner in
    settle_order ~owner { executed_order with size = partial_fill.filled_by };
    let remaining_size = Market.Size.O.(executed_order.size - partial_fill.filled_by) in
    Hashtbl.set owner.orders
      ~key:executed_order.id
      ~data:{ executed_order with size = remaining_size });
  Option.iter exec.posted ~f:(fun order ->
    Hashtbl.add_exn sender.orders ~key:order.id ~data:order);
  exec

let cancel t ~(id : Market.Order.Id.t) ~(sender : Player.t) =
  match t.stage with
  | Waiting_for_players -> Error `No_such_order
  | Playing round ->
  match Hashtbl.find sender.orders id with
  | None -> Error `No_such_order
  | Some order ->
    Result.map (Market.cancel round.market order) ~f:(fun new_market ->
      round.market <- new_market;
      Hashtbl.remove sender.orders id;
      order)
