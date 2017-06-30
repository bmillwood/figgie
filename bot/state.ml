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
  { mutable next_order_id : Order.Id.t
  ; orders                : Order_state.t Order.Id.Table.t
  ; mutable hand          : Size.t Card.Hand.t option
  } [@@deriving sexp_of]

let create () =
  { next_order_id = Order.Id.zero
  ; orders        = Order.Id.Table.create ()
  ; hand          = None
  }

let clear t =
  Hashtbl.clear t.orders;
  t.hand <- None

let send_order t ~conn ~(order : Order.t) =
  if Hashtbl.mem t.orders order.id then (
    return (Error `Duplicate_order_id)
  ) else (
    Hashtbl.set t.orders ~key:order.id ~data:(Order_state.of_order order);
    let%map r = Rpc.Rpc.dispatch_exn Protocol.Order.rpc conn order in
    Result.iter_error r ~f:(fun _ ->
        Hashtbl.remove t.orders order.id
      );
    r
  )

let ack _t ~order_state =
  Order_state.ack order_state

let out t ~id =
  Option.iter (Hashtbl.find_and_remove t.orders id)
    ~f:(fun order_state ->
        Order_state.out order_state;
      )

let fill t ~order_state ~size =
  let order = order_state.order in
  Order_state.fill order_state ~size;
  if Order_state.is_out order_state then (
    out t ~id:order.id
  );
  t.hand <- Option.map t.hand ~f:(fun hand ->
      Card.Hand.modify hand ~suit:order.symbol ~f:(fun amount ->
          let new_amount =
            Dir.fold order.dir ~buy:Size.(+) ~sell:Size.(-)
              amount size
          in
          if Size.(<) new_amount Size.zero then (
            raise_s [%message
              "Bot.State.fill: new position is negative"
                (t : t)
                (order_state : Order_state.t)
                (size : Size.t)
            ]
          ) else (
            new_amount
          )
        )
    )

let new_order_id t =
  let id = t.next_order_id in
  t.next_order_id <- Order.Id.next id;
  id
