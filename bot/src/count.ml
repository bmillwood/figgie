open Core
open Async

open Figgie
open Card
open Market

let consecutive_product from to_ =
  let rec go a f t =
    if f > t then a else go (a *. Int.to_float f) (f + 1) t
  in
  go 1. from to_

let choose n r =
  let n = Size.to_int n in
  let r = Size.to_int r in
  (* n! / r!(n - r)! *)
  let choose' n r =
    consecutive_product (r + 1) n
      /. consecutive_product 1 (n - r)
  in
  if r < 0 || r > n then (
    0.
  ) else if r * 2 > n then (
    choose' n r
  ) else (
    choose' n (n - r)
  )

let likelihood ~counts ~long ~short =
  let num_cards ~suit =
    Params.num_cards_in_role (Suit.role suit ~long ~short)
  in
  let sample_size =
    Hand.fold counts ~init:Size.zero ~f:Size.(+)
  in
  let is_impossible =
    List.exists Suit.all ~f:(fun suit ->
        Size.(>) (Hand.get counts ~suit) (num_cards ~suit)
      )
  in
  if is_impossible then (
    0.
  ) else if Size.(equal zero) sample_size then (
    1.
  ) else (
    let numerator =
      List.map Suit.all ~f:(fun suit ->
          choose (num_cards ~suit) (Hand.get counts ~suit)
        )
      |> List.fold ~init:1. ~f:( *. )
    in
    let denominator =
      choose Params.num_cards_in_deck sample_size
    in
    numerator /. denominator
  )

let total_num_cards_seen t =
  match Bot.players t with
  | None -> Hand.create_all Size.zero
  | Some players ->
    let hands =
      Map.data players
      |> List.map ~f:(fun p -> p.role.hand)
    in
    Hand.init ~f:(fun suit ->
        List.sum (module Size) hands ~f:(fun hand ->
            Hand.get hand.known ~suit
          )
      )

let likelihoods ~counts ?long ?short () =
  let or_all suit =
    Option.value_map suit ~default:Suit.all ~f:List.return
  in
  List.sum (module Float) (or_all long) ~f:(fun long ->
      List.sum (module Float) (or_all short) ~f:(fun short ->
          likelihood ~counts ~long ~short
        )
    )

let ps_gold ~counts =
  let p_counts = likelihoods ~counts () in
  if p_counts =. 0. then (
    raise_s [%message "these counts seem impossible"
      (counts : Size.t Hand.t)
    ]
  );
  Hand.init ~f:(fun suit ->
      likelihoods ~counts ~long:(Suit.opposite suit) () /. p_counts
    )

let trade_with t ~(order : Order.t) ~size =
  let order =
    Bot.Staged_order.create t
      ~symbol:order.symbol
      ~dir:(Dir.other order.dir)
      ~price:order.price
      ~size
  in
  match%bind Bot.Staged_order.send_exn order t with
  | Error (`Game_not_in_progress | `Not_enough_to_sell as e) ->
    Log.Global.sexp ~level:`Info
      [%message "Reject" (e : Protocol.Order.error)];
    Deferred.unit
  | Error e ->
    raise_s [%sexp (e : Protocol.Order.error)]
  | Ok `Ack -> Deferred.unit

let consider_halfbook t ~symbol ~dir ~counts ~ps halfbook =
  let max_can_trade_size =
    match (dir : Dir.t) with
    | Buy -> Card.Hand.get (Bot.sellable_hand t) ~suit:symbol
    | Sell -> Params.num_cards_in_deck
  in
  let p_gold = Card.Hand.get ps ~suit:symbol in
  let fair_without_pot_transfer =
    p_gold *. Price.to_float Params.gold_card_value
  in
  let good_to_fair px =
    Dir.fold dir ~buy:Float.(>=) ~sell:Float.(<=)
      px fair_without_pot_transfer
  in
  let candidates_to_trade_with, profit, size =
    let rec go cs profit size =
      function
      | [] -> (cs, profit, size)
      | o :: os ->
        if good_to_fair (Price.to_float o.Order.price) then (
          let this_size = Size.min o.size Size.O.(max_can_trade_size - size) in
          let profit =
            Size.to_float size
            *. Float.abs (Price.to_float o.price -. fair_without_pot_transfer)
            +. profit
          in
          let size = Size.O.(size + this_size) in
          if Size.equal size max_can_trade_size then (
            ({ o with size = this_size } :: cs, profit, size)
          ) else (
            go (o :: cs) profit size os
          )
        ) else (
          (cs, profit, size)
        )
    in
    go [] 0. Size.zero halfbook
  in
  let candidates_to_trade_with = List.rev candidates_to_trade_with in
  let expected_loss_if_losing_pot =
    match dir with
    | Buy ->
      let sellable_qty = Card.Hand.get (Bot.sellable_hand t) ~suit:symbol in
      let after_selling = Size.O.(sellable_qty - size) in
      if Size.(after_selling > of_int 5) then (
        0.
      ) else (
        let p_big_pot =
          likelihoods ~counts ~long:(Suit.opposite symbol) ~short:symbol ()
          /. likelihoods ~counts ~long:(Suit.opposite symbol) ()
        in
        p_big_pot *. 130. +. (1. -. p_big_pot) *. 110.
      )
    | Sell -> 0.
  in
  if profit >. p_gold *. expected_loss_if_losing_pot then (
    Deferred.List.iter ~how:`Sequential candidates_to_trade_with
      ~f:(fun order ->
        trade_with t ~order ~size:order.size
      )
  ) else (
    Deferred.unit
  )

let spec =
  Bot.Spec.create
    ~username_stem:"countbot"
    ~auto_ready:true
    (fun t ~config:() ->
      let username = Bot.username t in
      Pipe.iter (Bot.updates t) ~f:(function
        | Broadcast (Room_update (Exec exec)) ->
          let order = exec.order in
          begin if Username.equal order.owner username
          then
            Log.Global.sexp ~level:`Debug [%message
              "Saw my order" (order.dir : Dir.t) (order.symbol : Suit.t)]
          end;
          let counts = total_num_cards_seen t in
          Log.Global.sexp ~level:`Debug
            [%message "update"
              (counts : Market.Size.t Card.Hand.t)
              ~gold:(ps_gold ~counts : float Hand.t)
          ];
          Bot.request_update_exn t Market
        | Market book when List.is_empty (Bot.unacked_orders t) ->
          let counts = total_num_cards_seen t in
          let ps = ps_gold ~counts in
          Deferred.List.iter ~how:`Parallel Suit.all ~f:(fun suit ->
              let { Dirpair.buy; sell } = Hand.get book ~suit in
              let%bind () =
                consider_halfbook t ~symbol:suit ~dir:Buy ~counts ~ps buy
              and () =
                consider_halfbook t ~symbol:suit ~dir:Sell ~counts ~ps sell
              in
              return ()
            )
        | _ -> Deferred.unit))

let command =
  Bot.Spec.to_command spec
    ~summary:"Count cards"
    ~config_param:(Command.Param.return ())

let run = Bot.Spec.run spec ~config:()
