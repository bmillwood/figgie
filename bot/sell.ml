open Core.Std
open Async.Std

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
  and log_level = Client.log_level_flag
  in
  Log.Global.set_level log_level;
  { which
  ; initial_sell_price = Market.Price.of_int initial_sell_price
  ; fade = Market.Price.of_int fade
  ; size = Option.map size ~f:Market.Size.of_int
  }

let command =
  Client.make_command
    ~summary:"Offer all your cards at a fixed price"
    ~param
    ~username:(fun t -> Client.which_user ~stem:"sellbot" t.which)
    ~room_id:(fun _ -> Lobby.Room.Id.of_string "0")
    ~f:(fun client t ->
      let sell_prices =
        Card.Hand.init ~f:(fun _suit -> ref t.initial_sell_price)
      in
      let reset_sell_prices () =
        Card.Hand.iter sell_prices ~f:(fun r -> r := t.initial_sell_price)
      in
      let hand = ref (Card.Hand.create_all Market.Size.zero) in
      let sell ~suit ~size =
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
          | Error _ | Ok `Ack -> Deferred.unit
        end
      in
      let handle_my_filled_order ~suit (exec : Market.Exec.t) =
        let size =
          Market.Size.(+)
            (List.sum (module Market.Size) exec.fully_filled
              ~f:(fun order -> order.size))
            (Option.value_map exec.partially_filled
              ~default:Market.Size.zero
              ~f:(fun partial -> partial.filled_by))
        in
        sell ~suit ~size
      in
      let handle_exec (exec : Market.Exec.t) =
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
        | Broadcast (Round_over _) ->
          Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc client.conn true
          |> Deferred.ignore
        | Broadcast (Exec (order, exec)) ->
          if Username.equal order.owner client.username
          then handle_my_filled_order ~suit:order.symbol exec
          else handle_exec exec
        | Hand new_hand ->
          hand := new_hand;
          Log.Global.sexp ~level:`Debug
            [%sexp (new_hand : Market.Size.t Card.Hand.t)];
          (* The correctness of the below relies on sellbot never asking
             for a Hand update, only receiving them at the beginning of
             a new round. *)
          reset_sell_prices ();
          Deferred.List.iter ~how:`Parallel Card.Suit.all ~f:(fun suit ->
            let size =
              Option.value_map t.size
                ~default:Fn.id
                ~f:Market.Size.min
                (Card.Hand.get new_hand ~suit)
            in
            sell ~suit ~size)
        | _ -> Deferred.unit))
