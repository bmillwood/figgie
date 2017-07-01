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
  ; mutable room          : Lobby.Room.t option
  } [@@deriving sexp_of]

let create ~username =
  { username
  ; next_order_id = Order.Id.zero
  ; orders        = Order.Id.Table.create ()
  ; hand          = None
  ; room          = None
  }

let reset t =
  Hashtbl.clear t.orders;
  t.hand <- None

let hand_if_no_fills t = t.hand

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

let new_order_id t =
  let id = t.next_order_id in
  t.next_order_id <- Order.Id.next id;
  id

let send_order t ~conn ~(order : Order.t) =
  let send order =
    let order_state = Order_state.of_order order in
    match Hashtbl.add t.orders ~key:order.id ~data:order_state with
    | `Duplicate ->
      return (Error `Duplicate_order_id)
    | `Ok ->
      let%map r = Rpc.Rpc.dispatch_exn Protocol.Order.rpc conn order in
      Result.iter_error r ~f:(fun _ ->
          Order_state.out order_state;
          Hashtbl.remove t.orders order.id
        );
      r
  in
  let rec send_without_self_trade ~opposite_side (order : Order.t) =
    if Size.(equal zero) order.size then (
      return (Ok `Ack)
    ) else (
      match opposite_side with
      | [] -> send order
      | opp :: opps ->
        if Order.is_more_agg_or_equal order ~than:opp then (
          let wait_for_cancel =
            Rpc.Rpc.dispatch_exn Protocol.Cancel.rpc conn opp.id
            |> Deferred.ignore
          in
          let wait_for_order =
            match Ordering.of_int (Size.compare order.size opp.size) with
            | Less | Equal ->
              return (Ok `Ack)
            | Greater ->
              send_without_self_trade ~opposite_side:opps
                { order with size = Size.O.(order.size - opp.size) }
          in
          let%bind () = wait_for_cancel in
          wait_for_order
        ) else (
          send order
        )
    )
  in
  let opposite_side =
    Card.Hand.get (open_orders t) ~suit:order.symbol
    |> Dirpair.get ~dir:(Dir.other order.dir)
  in
  send_without_self_trade ~opposite_side order

let state_of_order t ~(order : Order.t) =
  if Username.equal order.owner t.username then (
    Some (Hashtbl.find_exn t.orders order.id)
  ) else (
    None
  )

let handle_out t ~order =
  Option.iter (state_of_order t ~order) ~f:(fun order_state ->
      Hashtbl.remove t.orders order.id;
      Order_state.out order_state
    )

let filled t ~fill =
  Option.iter (state_of_order t ~order:fill) ~f:(fun order_state ->
      let order = order_state.order in
      Order_state.fill order_state ~size:fill.size;
      if Order_state.is_out order_state then (
        handle_out t ~order
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

let handle_exec t ~(exec : Exec.t) =
  Option.iter (state_of_order t ~order:exec.order) ~f:Order_state.ack;
  let fills = Exec.fills exec in
  let total_qty = List.sum (module Size) fills ~f:(fun o -> o.size) in
  filled t ~fill:{ exec.order with size = total_qty };
  List.iter (Exec.fills exec) ~f:(fun fill ->
      filled t ~fill
    )

let handle_update t : Protocol.Game_update.t -> unit =
  function
  | Broadcast (Round_over _) ->
    reset t
  | Broadcast (Exec exec) ->
    handle_exec t ~exec
  | Broadcast (Out order) ->
    handle_out t ~order
  | Room_snapshot room ->
    t.room <- Some room
  | Broadcast (Room_update update) ->
    t.room <- Option.map t.room ~f:(Lobby.Room.Update.apply update)
  | Hand hand ->
    t.hand <- Some hand
  | _ -> ()

let room_with_my_hand t =
  Option.map t.room ~f:(fun room ->
      let with_my_hand room =
        match t.hand with
        | None -> room
        | Some hand ->
          Lobby.Room.Update.apply
            (Player_event
               { username = t.username
               ; event = Player_hand (Partial_hand.create_known hand)
               }
            )
            room
      in
      with_my_hand room
    )
