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

let max_loss_per_sell t ~symbol ~size =
  let might_lose_pot = 130. in
  match Bot.hand_if_filled t with
  | None -> might_lose_pot
  | Some hand ->
    let if_sold =
      Hand.map hand ~f:(Dirpair.get ~dir:Sell)
      |> Per_symbol.get ~symbol
    in
    if Size.(if_sold - size > of_int 5) then (
      10.
    ) else (
      might_lose_pot
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

let ps_gold t =
  let counts = total_num_cards_seen t in
  let likelihoods ?long ?short () =
    let or_all suit =
      Option.value_map suit ~default:Suit.all ~f:List.return
    in
    List.sum (module Float) (or_all long) ~f:(fun long ->
        List.sum (module Float) (or_all short) ~f:(fun short ->
            likelihood ~counts ~long ~short
          )
      )
  in
  let p_counts = likelihoods () in
  if p_counts =. 0. then (
    raise_s [%message "these counts seem impossible"
      (Bot.players t : Lobby.User.Player.t Username.Map.t option)
      (counts : Size.t Hand.t)
    ]
  );
  Hand.init ~f:(fun suit ->
      likelihoods ~long:(Suit.opposite suit) () /. p_counts
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
          Log.Global.sexp ~level:`Debug
            [%message "update"
              ~counts:(total_num_cards_seen t : Market.Size.t Card.Hand.t)
              ~gold:(ps_gold t : float Hand.t)
          ];
          Bot.request_update_exn t Market
        | Market book when List.is_empty (Bot.unacked_orders t) ->
          let ps = ps_gold t in
          Deferred.List.iter ~how:`Parallel Suit.all ~f:(fun suit ->
            let p_gold = Hand.get ps ~suit in
            let { Dirpair.buy; sell } = Hand.get book ~suit in
            Deferred.List.iter ~how:`Parallel
              (buy @ sell)
                ~f:(fun order ->
                  let size =
                    Size.min
                      order.size
                      (Hand.get (Bot.sellable_hand t) ~suit:order.symbol)
                  in
                  let want_to_trade =
                    match order.dir with
                    | Buy ->
                      let loss =
                        max_loss_per_sell t
                          ~symbol:order.symbol
                          ~size
                      in
                      loss *. p_gold <. Price.to_float order.price
                    | Sell ->
                      10. *. p_gold
                      >. Price.to_float order.price
                  in
                  if want_to_trade && Size.(>) size Size.zero
                  then begin
                    if Username.equal order.owner username
                    then begin
                      Bot.cancel t order.id
                      >>= function
                      | Error e ->
                        Log.Global.sexp ~level:`Error
                          [%sexp (e : Protocol.Cancel.error)];
                        Deferred.unit
                      | Ok `Ack -> Deferred.unit
                    end else begin
                      let order =
                        Bot.Staged_order.create t
                          ~symbol:order.symbol
                          ~dir:(Dir.other order.dir)
                          ~price:order.price
                          ~size:order.size
                      in
                      Bot.Staged_order.send_exn order t
                      >>= function
                      | Error (`Game_not_in_progress | `Not_enough_to_sell as e) ->
                        Log.Global.sexp ~level:`Info
                          [%message "Reject" (e : Protocol.Order.error)];
                        Deferred.unit
                      | Error e ->
                        raise_s [%sexp (e : Protocol.Order.error)]
                      | Ok _id -> Deferred.unit
                    end
                  end else Deferred.unit))
        | _ -> Deferred.unit))

let command =
  Bot.Spec.to_command spec
    ~summary:"Count cards"
    ~config_param:(Command.Param.return ())

let run = Bot.Spec.run spec ~config:()
