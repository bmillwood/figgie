open Core.Std
open Async.Std

let which_user ~stem i =
  stem ^ Option.value_map i ~default:"" ~f:Int.to_string
  |> Username.of_string

module Do_nothing = struct
  let param =
    let open Command.Param in
    flag "-which" (optional int)
      ~doc:"N modulate username"

  let command =
    ( "lazy"
    , Client.make_command
        ~summary:"Do nothing"
        ~param
        ~username:(fun i -> which_user ~stem:"lazybot" i)
        ~f:(fun client _which ->
          Pipe.iter client.updates ~f:(function
            | Round_over _ ->
              Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc client.conn true
              |> Deferred.ignore
            | _ -> Deferred.unit))
    )
end

module Offer_everything = struct
  type t = {
    which : int option;
    initial_sell_price : Market.Price.t;
    fade : Market.Price.t;
    size : Market.Size.t option;
  }

  let param =
    let open Command.Param in
    let open Command.Let_syntax in
    let%map which =
      flag "-which" (optional int)
        ~doc:"N modulate username"
    and initial_sell_price =
      flag "-at" (required int)
        ~doc:"P sell price"
    and fade =
      flag "-fade" (required int)
        ~doc:"F increase price after a sale"
    and size =
      flag "-size" (optional int)
        ~doc:"S sell at most S at a time"
    in
    { which
    ; initial_sell_price = Market.Price.of_int initial_sell_price
    ; fade = Market.Price.of_int fade
    ; size = Option.map size ~f:Market.Size.of_int
    }

  let command =
    ( "sell"
    , Client.make_command
        ~summary:"Offer all your cards at a fixed price"
        ~param
        ~username:(fun t -> which_user ~stem:"sellbot" t.which)
        ~f:(fun client t ->
          let sell_prices =
            Card.Hand.init ~f:(fun _suit -> ref t.initial_sell_price)
          in
          let reset_sell_prices () =
            Card.Hand.iter sell_prices ~f:(fun r -> r := t.initial_sell_price)
          in
          let hand = ref (Card.Hand.create_all Market.Size.zero) in
          let rec sell ~suit ~size =
            let size = Market.Size.min size (Card.Hand.get !hand ~suit) in
            if Market.Size.(equal zero) size
            then Deferred.unit
            else begin
              hand := Card.Hand.modify !hand ~suit
                ~f:(fun c -> Market.Size.O.(c - size));
              Rpc.Rpc.dispatch_exn Protocol.Order.rpc client.conn
                { owner = client.username
                ; id = client.new_order_id ()
                ; symbol = suit
                ; dir = Sell
                ; price = !(Card.Hand.get sell_prices ~suit)
                ; size
                }
              >>= function
              | Error _ -> Deferred.unit
              | Ok exec -> handle_exec exec
            end
          and handle_exec (exec : Market.Exec.t) =
            let amount_to_sell = ref Market.Size.zero in
            let suit_to_sell = ref None in
            let handle_filled_order (order : Market.Order.t) filled_amount =
              suit_to_sell := Some order.symbol;
              if Username.equal order.owner client.username
              then begin
                amount_to_sell :=
                  Market.Size.O.(!amount_to_sell
                    + filled_amount);
                let price_to_sell_at =
                  Card.Hand.get sell_prices ~suit:order.symbol
                in
                price_to_sell_at :=
                  Market.O.(Price.(!price_to_sell_at
                    + (filled_amount *$ t.fade)))
              end
            in
            List.iter exec.fully_filled ~f:(fun order ->
              handle_filled_order order order.size);
            Option.iter exec.partially_filled ~f:(fun partial_fill ->
              handle_filled_order partial_fill.original_order
                partial_fill.filled_by);
            begin match !suit_to_sell with
            | None -> Deferred.unit
            | Some suit -> sell ~suit ~size:!amount_to_sell
            end
          in
          Pipe.iter client.updates ~f:(function
            | Round_over _ ->
              Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc client.conn true
              |> Deferred.ignore
            | Dealt new_hand ->
              hand := new_hand;
              reset_sell_prices ();
              Deferred.List.iter ~how:`Parallel Card.Suit.all ~f:(fun suit ->
                let size =
                  Option.value_map t.size
                    ~default:Fn.id
                    ~f:Market.Size.min
                    (Card.Hand.get new_hand ~suit)
                in
                sell ~suit ~size)
            | Exec (order, exec) ->
              if not (Username.equal client.username order.owner)
              then handle_exec exec
              else Deferred.unit
            | _ -> Deferred.unit))
    )
end

module Card_counter = struct
  open Card
  open Market

  module Counts = struct
    type t = {
      per_player : Size.t Hand.t Username.Table.t
    } [@@deriving sexp]

    let create () = { per_player = Username.Table.create () }

    let clear t = Hashtbl.clear t.per_player

    let update t player ~f =
      Hashtbl.update t.per_player player ~f:(fun hand ->
        let hand = Option.value hand ~default:(Hand.create_all Size.zero) in
        f hand)

    let see_order t (order : Market.Order.t) =
      match order.dir with
      | Buy -> ()
      | Sell ->
        update t order.owner
          ~f:(Hand.modify ~suit:order.symbol ~f:(Size.max order.size))

    let see_exec t ~(order : Market.Order.t) (exec : Market.Exec.t) =
      let suit = order.symbol in
      let apply_fill ~(filled : Market.Order.t) ~size =
        update t filled.owner
          ~f:(Hand.modify ~suit ~f:(fun c ->
            Size.(+) c (Size.with_dir size ~dir:filled.dir)));
        update t order.owner
          ~f:(Hand.modify ~suit ~f:(fun c ->
            Size.(+) c (Size.with_dir size ~dir:order.dir)))
      in
      List.iter exec.fully_filled ~f:(fun filled ->
        apply_fill ~filled ~size:filled.size);
      Option.iter exec.partially_filled ~f:(fun pf ->
        apply_fill ~filled:pf.original_order ~size:pf.filled_by)

    let per_suit t ~suit =
      Hashtbl.fold t.per_player ~init:Size.zero
        ~f:(fun ~key:_ ~data:hand acc -> Size.(+) acc (Hand.get hand ~suit))

    let per_suits t = Hand.init ~f:(fun suit -> per_suit t ~suit)

    let p_gold t ~suit =
      let totals = per_suits t in
      let p_totals ?long ?short () =
        let or_all suit =
          Option.value_map suit ~default:Suit.all ~f:List.return
        in
        List.sum (module Float) (or_all long) ~f:(fun long ->
          List.sum (module Float) (or_all short) ~f:(fun short ->
            if List.exists Suit.all ~f:(fun suit ->
              Size.(>)
                (Hand.get totals ~suit)
                (Params.cards_in_suit suit ~long ~short))
            then 0.
            else begin
              let open Size.O in
              let prod min max =
                List.init (Size.to_int (max - min + Size.of_int 1))
                  ~f:(fun i -> min + Size.of_int i)
              in
              let sample_size = Hand.fold totals ~init:Size.zero ~f:Size.(+) in
              let numerator =
                prod (Size.of_int 1) sample_size
                @ List.concat_map Suit.all ~f:(fun suit ->
                  let num_cards = Params.cards_in_suit suit ~long ~short in
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
            end))
      in
      if p_totals () =. 0.
      then begin
        raise_s [%message "these counts seem impossible"
          ~counts:(t : t)
        ]
      end;
      let p_long ~long = p_totals ~long () /. p_totals () in
      p_long ~long:(Suit.opposite suit)

    let ps_gold t =
      Hand.init ~f:(fun suit -> p_gold t ~suit)
  end
  let param =
    let open Command.Param in
    flag "-which" (optional int)
      ~doc:"N modulate username"

  let command =
    ( "counter"
    , Client.make_command
        ~summary:"Count cards"
        ~param
        ~username:(fun i -> which_user ~stem:"countbot" i)
        ~f:(fun client _which ->
          let counts = Counts.create () in
          Pipe.iter client.updates ~f:(function
            | Round_over _ ->
              Counts.clear counts;
              Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc client.conn true
              |> Deferred.ignore
            | Dealt hand ->
              Counts.update counts client.username ~f:(Fn.const hand);
              Deferred.unit
            | Exec (order, exec) ->
              Counts.see_order counts order;
              Counts.see_exec  counts ~order exec;
              Log.Global.sexp ~level:`Info
                [%sexp (Counts.ps_gold counts : float Hand.t)];
              Rpc.Rpc.dispatch_exn Protocol.Book.rpc client.conn ()
              >>= fun book ->
              Deferred.List.iter ~how:`Parallel Suit.all ~f:(fun suit ->
                let fair = 10. *. Hand.get (Counts.ps_gold counts) ~suit in
                Deferred.List.iter ~how:`Parallel
                  (Hand.get book ~suit).sell ~f:(fun order ->
                    let want_to_trade =
                      match order.dir with
                      | Buy -> fair <. Price.to_float order.price
                      | Sell -> fair >. Price.to_float order.price
                    in
                    if want_to_trade
                    then begin
                      Rpc.Rpc.dispatch_exn Protocol.Order.rpc client.conn
                        { owner = client.username
                        ; id = client.new_order_id ()
                        ; symbol = order.symbol
                        ; dir = Dir.other order.dir
                        ; price = order.price
                        ; size = order.size
                        }
                      >>= function
                      | Error e ->
                        raise_s [%sexp (e : Protocol.Order.error)]
                      | Ok _exec -> Deferred.unit
                    end else Deferred.unit))
            | _ -> Deferred.unit))
    )
end

let command =
  Command.group
    ~summary:"Run a bot"
    [ Do_nothing.command
    ; Offer_everything.command
    ; Card_counter.command
    ]
