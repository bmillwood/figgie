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
        ~f:(fun t _which -> Pipe.closed t.updates)
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

let command =
  Command.group
    ~summary:"Run a bot"
    [ Do_nothing.command
    ; Offer_everything.command
    ]
