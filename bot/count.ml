open Core
open Async

open Figgie
open Card
open Market

let total_num_cards_seen t =
  match Bot.room_with_my_hand t with
  | None -> Hand.create_all Size.zero
  | Some room ->
    let hands =
      Lobby.Room.users room
      |> Map.filter_map ~f:(fun (user : Lobby.User.t) ->
          match user.role with
          | Player p -> Some p.hand
          | Observer _ -> None
        )
      |> Map.data
    in
    Hand.init ~f:(fun suit ->
        List.sum (module Size) hands ~f:(fun hand ->
            Hand.get hand.known ~suit
          )
      )

let p_gold t ~suit =
  let totals = total_num_cards_seen t in
  let p_totals ?long ?short () =
    let or_all suit =
      Option.value_map suit ~default:Suit.all ~f:List.return
    in
    List.sum (module Float) (or_all long) ~f:(fun long ->
        List.sum (module Float) (or_all short) ~f:(fun short ->
            let sample_size =
              Hand.fold totals ~init:Size.zero ~f:Size.(+)
            in
            let is_impossible =
              List.exists Suit.all ~f:(fun suit ->
                  Size.(>)
                    (Hand.get totals ~suit)
                    (Params.cards_in_suit suit ~long ~short))
            in
            if is_impossible then (
              0.
            ) else if Size.(equal zero) sample_size then (
              1.
            ) else (
              let open Size.O in
              let prod min max =
                List.init (Size.to_int (max - min + Size.of_int 1))
                  ~f:(fun i -> min + Size.of_int i)
              in
              let numerator =
                prod (Size.of_int 1) sample_size
                @ List.concat_map Suit.all ~f:(fun suit ->
                    let num_cards =
                      Params.cards_in_suit suit ~long ~short
                    in
                    prod
                      (num_cards - Hand.get totals ~suit + Size.of_int 1)
                      num_cards)
              in
              let denominator =
                prod
                  (Params.num_cards_in_deck - sample_size + Size.of_int 1)
                  Params.num_cards_in_deck
                @ List.concat_map Suit.all ~f:(fun suit ->
                    prod (Size.of_int 1) (Hand.get totals ~suit))
              in
              List.reduce_balanced_exn ~f:( *. )
                (List.map numerator ~f:Size.to_float)
              /. List.reduce_balanced_exn ~f:( *. )
                (List.map denominator ~f:Size.to_float)
            )
          )
      )
  in
  if p_totals () =. 0. then (
    raise_s [%message "these counts seem impossible"
      (Bot.room_with_my_hand t : Lobby.Room.t option)
      (totals : Size.t Hand.t)
    ]
  );
  let p_long ~long = p_totals ~long () /. p_totals () in
  p_long ~long:(Suit.opposite suit)

let ps_gold t =
  Hand.init ~f:(fun suit -> p_gold t ~suit)

let command =
  Bot.make_command
    ~summary:"Count cards"
    ~config_param:(Command.Param.return ())
    ~username_stem:"countbot"
    ~auto_ready:true
    (fun t ~config:() ->
      let username = Bot.username t in
      Pipe.iter (Bot.updates t) ~f:(function
        | Broadcast (Exec exec) ->
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
                  let want_to_trade =
                    (* countbot doesn't understand the value of getting
                       the most gold cards, so let's do the maximally
                       conservative thing and assume (a) the pot is
                       120 (8-card gold suit) and (b) this sale is losing
                       us the pot (plus the 10 points per card). *)
                    match order.dir with
                    | Buy -> 130. *. p_gold <. Price.to_float order.price
                    | Sell -> 10. *. p_gold >. Price.to_float order.price
                  in
                  let size =
                    Size.min
                      order.size
                      (Hand.get (Bot.sellable_hand t) ~suit:order.symbol)
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
