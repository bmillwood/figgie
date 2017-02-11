open Core.Std
open Async.Std

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

  let see_order t (order : Order.t) =
    match order.dir with
    | Buy -> ()
    | Sell ->
      update t order.owner
        ~f:(Hand.modify ~suit:order.symbol ~f:(Size.max order.size))

  let see_exec t ~(order : Order.t) (exec : Exec.t) =
    let suit = order.symbol in
    let apply_fill ~(filled : Order.t) ~size =
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
  let open Command.Let_syntax in
  let%map_open which =
    flag "-which" (optional int)
      ~doc:"N modulate username"
  and log_level = Client.log_level_flag
  in
  Log.Global.set_level log_level;
  which

let command =
  Client.make_command
    ~summary:"Count cards"
    ~param
    ~username:(fun i -> Client.which_user ~stem:"countbot" i)
    ~room_id:(fun _ -> Lobby.Room.Id.of_string "0")
    ~f:(fun client _which ->
      let counts = Counts.create () in
      let pending_ack = ref None in
      Pipe.iter client.updates ~f:(function
        | Broadcast (Round_over results) ->
          Counts.clear counts;
          Log.Global.sexp ~level:`Info [%message "round over"
            (results.gold : Suit.t)
          ];
          Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc client.conn true
          |> Deferred.ignore
        | Broadcast (Exec (order, exec)) ->
          if Option.exists !pending_ack ~f:(Order.Id.equal order.id)
          then (
            pending_ack := None
          );
          Counts.see_order counts order;
          Counts.see_exec  counts ~order exec;
          begin if Username.equal order.owner client.username
          then
            Log.Global.sexp ~level:`Debug [%message
              "My order" (order.dir : Dir.t) (order.symbol : Suit.t)]
          end;
          Log.Global.sexp ~level:`Debug
            [%sexp (Counts.ps_gold counts : float Hand.t)];
          Rpc.Rpc.dispatch_exn Protocol.Get_update.rpc client.conn Market
          >>| Protocol.playing_exn
        | Hand hand ->
          Counts.update counts client.username ~f:(Fn.const hand);
          Deferred.unit
        | Market book when Option.is_none !pending_ack ->
          Deferred.List.iter ~how:`Parallel Suit.all ~f:(fun suit ->
            let p_gold = Hand.get (Counts.ps_gold counts) ~suit in
            let { Dirpair.buy; sell } = Hand.get book ~suit in
            Deferred.List.iter ~how:`Parallel
              (buy @ sell)
                ~f:(fun order ->
                  let want_to_trade =
                    (* countbot doesn't understand the value of getting
                       the most gold cards, so let's do the maximally
                       conservative thing and assume (a) the pot is
                       120 (8-card gold suit) and (b) this sale is taking
                       us from first to a four-way pot split, losing
                       three-quarters of the pot and the 10 for the gold
                       card. Future work: use knowledge of our own and
                       others' hand sizes to bound loss further. *)
                    match order.dir with
                    | Buy -> 100. *. p_gold <. Price.to_float order.price
                    | Sell -> 10. *. p_gold >. Price.to_float order.price
                  in
                  if want_to_trade
                  then begin
                    if Username.equal order.owner client.username
                    then begin
                      Rpc.Rpc.dispatch_exn Protocol.Cancel.rpc client.conn order.id
                      >>= function
                      | Error e ->
                        Log.Global.sexp ~level:`Error [%sexp (e : Protocol.Cancel.error)];
                        Deferred.unit
                      | Ok `Ack -> Deferred.unit
                    end else begin
                      let id = client.new_order_id () in
                      pending_ack := Some id;
                      Rpc.Rpc.dispatch_exn Protocol.Order.rpc client.conn
                        { owner = client.username
                        ; id
                        ; symbol = order.symbol
                        ; dir = Dir.other order.dir
                        ; price = order.price
                        ; size = order.size
                        }
                      >>= function
                      | Error e ->
                        raise_s [%sexp (e : Protocol.Order.error)]
                      | Ok `Ack -> Deferred.unit
                    end
                  end else Deferred.unit))
        | _ -> Deferred.unit))
