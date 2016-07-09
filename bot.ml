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
    at : Market.Price.t;
    size : Market.Size.t option;
  }

  let param =
    let open Command.Param in
    let open Command.Let_syntax in
    let%map which =
      flag "-which" (optional int)
        ~doc:"N modulate username"
    and at =
      flag "-at" (required int)
        ~doc:"P sell price"
    and size =
      flag "-size" (optional int)
        ~doc:"S sell at most S at a time"
    in
    { which
    ; at = Market.Price.of_int at
    ; size = Option.map size ~f:Market.Size.of_int
    }

  let command =
    ( "sell"
    , Client.make_command
        ~summary:"Offer all your cards at a fixed price"
        ~param
        ~username:(fun t -> which_user ~stem:"sellbot" t.which)
        ~f:(fun client t ->
          let hand = ref (Card.Hand.create_all Market.Size.zero) in
          let rec sell ~suit ~size =
            let size = Market.Size.max size (Card.Hand.get !hand ~suit) in
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
                ; price = t.at
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
              then amount_to_sell :=
                Market.Size.O.(!amount_to_sell + filled_amount)
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
            | Dealt hand ->
              Deferred.List.iter ~how:`Parallel Card.Suit.all ~f:(fun suit ->
                let size =
                  Option.value_map t.size
                    ~default:Fn.id
                    ~f:Market.Size.max
                    (Card.Hand.get hand ~suit)
                in
                sell ~suit ~size)
            | _ -> Deferred.unit))
    )
end

module Card_counter = struct
  open Card
  open Market

  module Counts = struct
    type t = {
      per_player : Size.t Hand.t Username.Table.t
    }

    let create () = { per_player = Username.Table.create () }

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
                prod (num_cards - Hand.get totals ~suit) num_cards)
            in
            let denominator =
              prod
                (Params.num_cards_in_deck - sample_size)
                Params.num_cards_in_deck
              @ List.concat_map Suit.all ~f:(fun suit ->
                prod (Size.of_int 1) (Hand.get totals ~suit))
            in
            List.reduce_balanced_exn ~f:( *. )
              (List.map numerator ~f:Size.to_float)
            /. List.reduce_balanced_exn ~f:( *. )
              (List.map denominator ~f:Size.to_float)))
      in
      let prior ~long:_ = 0.25 in
      let p_long ~long =
        prior ~long *. p_totals ~long () /. p_totals ()
      in
      p_long ~long:(Suit.opposite suit)
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
          Pipe.iter client.updates ~f:(function
            | Round_over _ ->
              Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc client.conn true
              |> Deferred.ignore
            | _ -> Deferred.unit))
    )
end

let command =
  Command.group
    ~summary:"Run a bot"
    [ Do_nothing.command
    ; Offer_everything.command
    ]
