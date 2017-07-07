open Core
open Result.Monad_infix

open Figgie

module Config = struct
  type t = { length_of_round : Time_ns.Span.t }

  let arg =
    let open Command.Let_syntax in
    [%map_open
      let length_of_round =
        flag "-length-of-round"
          (required (Arg_type.create Time_ns.Span.of_string))
          ~doc:"SPAN duration of a single game round"
      in
      { length_of_round }
    ]
end

module Player = struct
  type t = {
    username : Username.t;
  } [@@deriving sexp]
end

module Round = struct
  module Player = struct
    type t = {
      p : Player.t;
      mutable hand : Market.Size.t Card.Hand.t;
      orders : Market.Order.t Market.Order.Id.Table.t;
    } [@@deriving sexp]

    let create p =
      { p
      ; hand  = Card.Hand.create_all Market.Size.zero
      ; orders  = Market.Order.Id.Table.create ()
      }

    let sellable_hand t =
      Hashtbl.fold t.orders ~init:t.hand ~f:(fun ~key:_ ~data:order acc ->
        match order.dir with
        | Sell ->
          Card.Hand.modify acc ~suit:order.symbol
            ~f:(fun s -> Market.Size.O.(s - order.size))
        | Buy -> acc
      )
  end

  type t = {
    players : Player.t Username.Map.t;
    mutable market : Market.Book.t;
    gold : Card.Suit.t;
    end_time : Time_ns.t;
  }

  let end_time t = t.end_time

  let market t = t.market

  let results t : Lobby.Room.Update.Round_results.t =
    { gold = t.gold
    ; hands = Map.map t.players ~f:(fun p -> p.hand)
    }

  let start ~(config : Config.t) ~players =
    let num_players = Map.length players in
    assert (Market.Size.to_int Params.num_cards_in_deck mod num_players = 0);
    let hands, gold =
      let open Card in
      let long, short = Suit.random_two () in
      let deck =
        Hand.init ~f:(fun _ -> ref (Params.num_cards_in_role Normal))
      in
      Hand.get deck ~suit:long  := Params.num_cards_in_role Long;
      Hand.get deck ~suit:short := Params.num_cards_in_role Short;
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
    let players =
      List.map2_exn (Map.data players) (Array.to_list hands) ~f:(fun p hand ->
        let player = Player.create p in
        player.hand <- hand;
        p.username, player)
      |> Username.Map.of_alist_exn
    in
    (* Note we don't actually schedule anything to happen at this time.
       That's done by the server, for reasons. *)
    let end_time = Time_ns.(add (now ()) config.length_of_round) in
    { players; market = Market.Book.empty; gold; end_time }

  let add_player_order t
    ~order:(sent_order : Market.Order.t)
    ~(sender : Player.t) =
    Result.ok_if_true (Username.equal sent_order.owner sender.p.username)
      ~error:`Owner_is_not_sender
    >>= fun () ->
    Result.ok_if_true (not (Hashtbl.mem sender.orders sent_order.id))
      ~error:`Duplicate_order_id
    >>= fun () ->
    Params.validate_price sent_order.price
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
    let { Market.Match_result.exec; new_market } =
      Market.Book.match_ t.market sent_order
    in
    t.market <- new_market;
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
      add_cards ~player:owner  size_for_owner;
      add_cards ~player:sender (Market.Size.neg size_for_owner);
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

  let cancel_player_order t ~(id : Market.Order.Id.t) ~(sender : Player.t) =
    match Hashtbl.find sender.orders id with
    | None -> Error `No_such_order
    | Some order ->
      Result.map (Market.Book.cancel t.market order) ~f:(fun new_market ->
        t.market <- new_market;
        Hashtbl.remove sender.orders id;
        order)

  let cancel_player_orders t ~(sender : Player.t) =
    let orders = Hashtbl.data sender.orders in
    List.iter orders ~f:(fun order ->
      match Market.Book.cancel t.market order with
      | Error `No_such_order ->
        raise_s [%message "order table order not in market"
          (sender : Player.t)
          (order.id : Market.Order.Id.t)
          (t.market : Market.Book.t)
        ]
      | Ok new_market ->
        t.market <- new_market;);
    Hashtbl.clear sender.orders;
    Ok orders

  let with_player t ~username ~f =
    match Map.find t.players username with
    | None -> Error `You're_not_playing
    | Some player -> f ~player

  let get_hand t ~username =
    with_player t ~username ~f:(fun ~player -> Ok player.hand)

  let add_order t ~order ~sender:username =
    with_player t ~username ~f:(fun ~player ->
      add_player_order t ~order ~sender:player)

  let cancel_order t ~id ~sender:username =
    with_player t ~username ~f:(fun ~player ->
      cancel_player_order t ~id ~sender:player)

  let cancel_orders t ~sender:username =
    with_player t ~username ~f:(fun ~player ->
      cancel_player_orders t ~sender:player)
end

module Phase = struct
  type t =
    | Waiting_for_players
    | Playing of Round.t
end

type t =
  { config : Config.t
  ; mutable phase : Phase.t
  }

let phase t = t.phase

let create ~config =
  { config
  ; phase = Waiting_for_players
  }

let start_round t ~room =
  let round_players =
    Map.map (Lobby.Room.players room) ~f:(fun player ->
        { Player.username = player.username }
      )
  in
  let round = Round.start ~config:t.config ~players:round_players in
  t.phase <- Playing round;
  round

let end_round t (round : Round.t) =
  t.phase <- Waiting_for_players;
  Round.results round
