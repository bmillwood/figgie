open Core
open Async

open Figgie
open Market

module Order_state = struct
  type t =
    { order : Order.t
    ; mutable is_acked : bool
    ; mutable size_filled : Size.t
    ; mutable size_open   : Size.t
    } [@@deriving sexp_of]

  let of_order order =
    { order
    ; is_acked = false
    ; size_filled = Size.zero
    ; size_open = order.size
    }

  let ack t = t.is_acked <- true

  let fill t ~size =
    t.size_filled <- Size.(+) t.size_filled size;
    t.size_open   <- Size.(-) t.size_open   size

  let out t = t.size_open <- Size.zero

  let is_out t = Size.(equal zero) t.size_open
end
open Order_state

type t =
  { username              : Username.t
  ; mutable next_order_id : Order.Id.t
  ; orders                : Order_state.t Order.Id.Table.t
  ; mutable hand          : Size.t Card.Hand.t option
  } [@@deriving sexp_of]

let create ~username =
  { username
  ; next_order_id = Order.Id.zero
  ; orders        = Order.Id.Table.create ()
  ; hand          = None
  }

let reset t =
  Hashtbl.clear t.orders;
  t.hand <- None

let hand_if_no_fills t = t.hand

let set_hand t ~hand = t.hand <- Some hand

let unacked_orders t =
  Hashtbl.fold t.orders
    ~init:[]
    ~f:(fun ~key:_ ~data:order_state acc ->
        if order_state.is_acked then (
          acc
        ) else (
          order_state.order :: acc
        )
      )

let open_orders t =
  Hashtbl.fold t.orders
    ~init:(Card.Hand.create_all (Dirpair.create_both []))
    ~f:(fun ~key:_ ~data:order_state orders ->
        let order = order_state.order in
        Card.Hand.modify orders ~suit:order.symbol
          ~f:(Dirpair.modify ~dir:order.dir ~f:(fun halfbook ->
              Halfbook.add_order halfbook order
            ))
      )

let send_order t ~conn ~(order : Order.t) =
  let order_state = Order_state.of_order order in
  match Hashtbl.add t.orders ~key:order.id ~data:order_state with
  | `Duplicate ->
    return (Error `Duplicate_order_id)
  | `Ok ->
    let%map r = Rpc.Rpc.dispatch_exn Protocol.Order.rpc conn order in
    Result.iter_error r ~f:(fun _ ->
        Hashtbl.remove t.orders order.id
      );
    r

let state_of_order t ~(order : Order.t) =
  if Username.equal order.owner t.username then (
    Some (Hashtbl.find_exn t.orders order.id)
  ) else (
    None
  )

let ack t ~order =
  Option.iter (state_of_order t ~order) ~f:Order_state.ack

let out t ~order =
  Option.iter (state_of_order t ~order) ~f:(fun order_state ->
      Hashtbl.remove t.orders order.id;
      Order_state.out order_state
    )

let filled t ~fill =
  Option.iter (state_of_order t ~order:fill) ~f:(fun order_state ->
      let order = order_state.order in
      Order_state.fill order_state ~size:fill.size;
      if Order_state.is_out order_state then (
        out t ~order
      );
      t.hand <- Option.map t.hand ~f:(fun hand ->
          Card.Hand.modify hand ~suit:order.symbol ~f:(fun amount ->
              let new_amount =
                Dir.fold order.dir ~buy:Size.(+) ~sell:Size.(-)
                  amount fill.size
              in
              if Size.(<) new_amount Size.zero then (
                raise_s [%message
                  "Bot.State.fill: new position is negative"
                    (hand : Size.t Card.Hand.t)
                    (order_state : Order_state.t)
                    (fill.size : Size.t)
                ]
              ) else (
                new_amount
              )
            )
        )
    )

let exec t ~(exec : Exec.t) =
  ack t ~order:exec.order;
  let fills = Exec.fills exec in
  let total_qty = List.sum (module Size) fills ~f:(fun o -> o.size) in
  filled t ~fill:{ exec.order with size = total_qty };
  List.iter (Exec.fills exec) ~f:(fun fill ->
      filled t ~fill
    )

let new_order_id t =
  let id = t.next_order_id in
  t.next_order_id <- Order.Id.next id;
  id
