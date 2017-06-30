open Core
open Async

open Figgie
open Market

let bernoulli ~p =
  Random.int (Rational.den p) < Rational.num p

let binomial ~n ~p =
  Fn.apply_n_times ~n (fun t -> t + if bernoulli ~p then 1 else 0) 0

let half = Rational.(of_int 1 / 2)

module Config = struct
  type t =
    { aggression : Rational.t
    ; mean_chaos_interval : Time.Span.t
    }

  let param =
    let rational = Command.Param.sexp_conv Rational.t_of_sexp in
    let open Command.Let_syntax in
    [%map_open
      let aggression =
        flag "-aggression"
          (optional_with_default half rational)
          ~doc:"N/D how often to send orders more agg than last"
      and mean_chaos_interval =
        flag "-mean-interval"
          (optional_with_default (sec 3.) time_span)
          ~doc:"SPAN average time between orders"
      in
      { aggression
      ; mean_chaos_interval
      }
    ]
end
open Config

let chaos_interval config =
  Time.Span.scale config.mean_chaos_interval
    (Float.of_int (binomial ~n:100 ~p:half) /. 50.)

let rec chaos_loop t ~config ~lasts =
  let%bind () = Clock.after (chaos_interval config) in
  let suit = List.random_element_exn Card.Suit.all in
  let position = Card.Hand.get (Bot.sellable_hand t) ~suit in
  let last = !(Card.Hand.get lasts ~suit) in
  let price =
    let l = Price.to_rational last in
    let n =
      binomial
        ~n:(Rational.num l * 2)
        ~p:half
    in
    let d = Rational.den l in
    Price.(of_int n / d)
  in
  let dir =
    if Size.O.(position > zero) then (
      let is_aggressive = bernoulli ~p:config.aggression in
      let passive_dir =
        match Ordering.of_int (Price.compare price last) with
        | Less -> Dir.Buy
        | Greater -> Dir.Sell
        | Equal -> List.random_element_exn Dir.[Buy; Sell]
      in
      passive_dir
      |> if is_aggressive then Dir.other else Fn.id
    ) else (
      Dir.Buy
    )
  in
  let size = Size.of_int 1 in
  match%bind
    Bot.Staged_order.send_exn
      (Bot.Staged_order.create t
         ~symbol:suit
         ~dir
         ~price
         ~size)
      t
  with
  | Error _ | Ok `Ack -> chaos_loop t ~config ~lasts

let price_of_fills (fills : Order.t list) =
  (List.hd_exn fills).price

let command =
  Bot.make_command
    ~summary:"Send orders randomly around last"
    ~config_param:Config.param
    ~username_stem:"chaosbot"
    ~auto_ready:true
    (fun t ~config ->
        Random.self_init ();
        let lasts = Card.Hand.init ~f:(fun _ -> ref (Price.of_int 5)) in
        don't_wait_for (
          chaos_loop t ~config ~lasts
        );
        Pipe.iter (Bot.updates t) ~f:(function
          | Broadcast (Round_over _results) ->
            Card.Hand.iter lasts ~f:(fun r -> r := Price.of_int 5);
            Deferred.unit
          | Broadcast (Exec exec) ->
            let fills = Exec.fills exec in
            if not (List.is_empty fills) then (
              let last = Card.Hand.get lasts ~suit:exec.order.symbol in
              last := price_of_fills fills;
            );
            Bot.request_update_exn t Hand
          | _ ->
            Deferred.unit
          )
      )
