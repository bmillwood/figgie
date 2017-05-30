open Core
open Async

open Figgie
open Card
open Market

module Counts = struct
  type t = {
    per_player : Partial_hand.t Username.Table.t
  } [@@deriving sexp_of]

  let create () = { per_player = Username.Table.create () }

  let clear t = Hashtbl.clear t.per_player

  let update t player ~f =
    Hashtbl.update t.per_player player ~f:(fun hand ->
      let hand =
        Option.value hand
          ~default:(Partial_hand.create_unknown Params.num_cards_per_hand)
      in
      f hand)

  let see_order t (order : Order.t) =
    match order.dir with
    | Buy -> ()
    | Sell ->
      update t order.owner
        ~f:(Partial_hand.selling ~suit:order.symbol ~size:order.size)

  let see_exec t ~(order : Order.t) (exec : Exec.t) =
    let suit = order.symbol in
    List.iter (Exec.fills exec) ~f:(fun filled ->
        let size = filled.size in
        update t filled.owner
          ~f:(Partial_hand.traded ~suit ~size ~dir:filled.dir);
        update t order.owner
          ~f:(Partial_hand.traded ~suit ~size ~dir:order.dir)
      )

  let per_suit t ~suit =
    Hashtbl.fold t.per_player ~init:Size.zero
      ~f:(fun ~key:_ ~data:hand acc ->
        Size.(+) acc (Hand.get hand.known ~suit))

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

let command =
  Bot.make_command
    ~summary:"Count cards"
    ~config_param:(Command.Param.return ())
    ~username_stem:"countbot"
    ~auto_ready:true
    (fun t ~config:() ->
      let username = Bot.username t in
      let counts = Counts.create () in
      let pending_ack = ref None in
      Pipe.iter (Bot.updates t) ~f:(function
        | Broadcast (Round_over results) ->
          Counts.clear counts;
          pending_ack := None;
          Log.Global.sexp ~level:`Info [%message "round over"
            (results.gold : Suit.t)
          ];
          Bot.try_set_ready t
        | Broadcast (Exec exec) ->
          let order = exec.order in
          if Option.exists !pending_ack ~f:(Order.Id.equal order.id)
          then (
            pending_ack := None
          );
          Counts.see_order counts order;
          Counts.see_exec  counts ~order exec;
          begin if Username.equal order.owner username
          then
            Log.Global.sexp ~level:`Debug [%message
              "My order" (order.dir : Dir.t) (order.symbol : Suit.t)]
          end;
          Log.Global.sexp ~level:`Debug
            [%message "update"
              ~counts:(Counts.per_suits counts : Market.Size.t Card.Hand.t)
              ~gold:(Counts.ps_gold counts : float Hand.t)
          ];
          Bot.request_update_exn t Market
        | Hand hand ->
          Counts.update counts username
            ~f:(fun _ -> Partial_hand.create_known hand);
          Deferred.unit
        | Market book when Option.is_none !pending_ack ->
          let ps = Counts.ps_gold counts in
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
                       120 (8-card gold suit) and (b) this sale is taking
                       us from first to a four-way pot split, losing
                       three-quarters of the pot and the 10 for the gold
                       card. Future work: use knowledge of our own and
                       others' hand sizes to bound loss further. *)
                    (* countbot also doesn't know its own position, so the sell
                       can fail. That's fine. It ignores the reject. *)
                    match order.dir with
                    | Buy -> 100. *. p_gold <. Price.to_float order.price
                    | Sell -> 10. *. p_gold >. Price.to_float order.price
                  in
                  if want_to_trade
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
                      pending_ack := Some (Bot.Staged_order.id order);
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
