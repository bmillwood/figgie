open Core
open Async

open Figgie
open Market

module Room_choice = struct
  type t =
    | First_available
    | Named of Lobby.Room.Id.t
    [@@deriving sexp]

  let param =
    let open Command.Param in
    flag "-room"
      (optional_with_default First_available
        (Arg_type.create (fun s -> Named (Lobby.Room.Id.of_string s))))
      ~doc:"ID room to join on startup, defaults to first available"
end

module State = struct
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
end

type t =
  { username : Username.t
  ; conn     : Rpc.Connection.t
  ; updates  : Protocol.Game_update.t Pipe.Reader.t
  ; state    : State.t
  }

let username t = t.username
let updates  t = t.updates

let unacked_orders t =
  Hashtbl.fold t.state.orders
    ~init:[]
    ~f:(fun ~key:_ ~data:order_state acc ->
        if order_state.is_acked then (
          acc
        ) else (
          order_state.order :: acc
        )
      )

let open_orders t =
  Hashtbl.fold t.state.orders
    ~init:(Card.Hand.create_all (Dirpair.create_both []))
    ~f:(fun ~key:_ ~data:order_state orders ->
        let order = order_state.order in
        Card.Hand.modify orders ~suit:order.symbol
          ~f:(Dirpair.modify ~dir:order.dir ~f:(fun halfbook ->
              Halfbook.add_order halfbook order
            ))
      )

let hand_if_no_fills t = t.state.hand

let hand_if_filled t =
  Option.map (hand_if_no_fills t) ~f:(fun hand_if_no_fills ->
      Card.Hand.map2
        hand_if_no_fills
        (open_orders t)
        ~f:(fun from_hand open_orders ->
            Dirpair.mapi open_orders ~f:(fun dir orders ->
                let total_size =
                  List.sum (module Size) orders ~f:(fun o -> o.size)
                in
                Dir.fold dir ~buy:Size.(+) ~sell:Size.(-)
                  from_hand total_size
              )
          )
    )

let sellable_hand t =
  match hand_if_filled t with
  | None -> Card.Hand.create_all Size.zero
  | Some hif -> Card.Hand.map hif ~f:(Dirpair.get ~dir:Sell)

module Staged_order = struct
  type t = Order.t

  let create bot ~symbol ~dir ~price ~size : t =
    { owner = username bot
    ; id = State.new_order_id bot.state
    ; symbol; dir; price; size
    }

  let id (t : t) = t.id

  let send_exn t bot =
    State.send_order bot.state ~conn:bot.conn ~order:t
end

let cancel t id = Rpc.Rpc.dispatch_exn Protocol.Cancel.rpc t.conn id

let request_update_exn t thing_to_get =
  match%map Rpc.Rpc.dispatch_exn Protocol.Get_update.rpc t.conn thing_to_get with
  | Error (`Not_logged_in | `Not_in_a_room | `You're_not_playing) -> assert false
  | Error `Game_not_in_progress
  | Ok () -> ()

let try_set_ready_on_conn ~conn =
  Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc conn true
  |> Deferred.ignore

let try_set_ready t = try_set_ready_on_conn ~conn:t.conn

let join_any_room ~conn ~username =
  let%bind lobby_updates =
    match%map
      Rpc.Pipe_rpc.dispatch Protocol.Get_lobby_updates.rpc conn ()
      >>| ok_exn
    with
    | Error `Not_logged_in -> assert false
    | Ok (lobby_updates, _metadata) -> lobby_updates
  in
  let can_join room =
    not (Lobby.Room.is_full room) || Lobby.Room.has_player room ~username
  in
  let try_to_join id =
    match%map
      Rpc.Pipe_rpc.dispatch Protocol.Join_room.rpc conn id
      >>| ok_exn
    with
    | Ok (updates, _pipe_metadata) ->
      Pipe.close_read lobby_updates;
      `Finished (id, updates)
    | Error (`Already_in_a_room | `Not_logged_in) -> assert false
    | Error `No_such_room -> `Repeat ()
  in
  Deferred.repeat_until_finished ()
    (fun () ->
      let%bind update =
        match%map Pipe.read lobby_updates with
        | `Eof -> failwith "Server hung up on us"
        | `Ok update -> update
      in
      match update with
      | Lobby_snapshot lobby ->
        begin match
            List.find (Map.to_alist lobby.rooms) ~f:(fun (_id, room) ->
                can_join room)
          with
          | Some (id, room) ->
            Log.Global.sexp ~level:`Debug [%message
              "doesn't look full, joining"
                (id : Lobby.Room.Id.t) (room : Lobby.Room.t)
            ];
            try_to_join id
          | None ->
            Log.Global.sexp ~level:`Debug [%message
              "couldn't see an empty room, waiting"
            ];
            return (`Repeat ())
        end
      | Lobby_update (New_empty_room { room_id }) ->
        try_to_join room_id
      | Lobby_update (Lobby_update _ | Room_closed _ | Room_update _)
      | Chat _ -> return (`Repeat ())
    )

let start_playing ~conn ~username ~(room_choice : Room_choice.t) =
  let%bind () =
    match%map Rpc.Rpc.dispatch_exn Protocol.Login.rpc conn username with
    | Error (`Already_logged_in | `Invalid_username) -> assert false
    | Ok () -> ()
  in
  let%bind (_room_id, updates) =
    match room_choice with
    | First_available ->
      join_any_room ~conn ~username
    | Named id ->
      let%map (updates, _metadata) =
        Rpc.Pipe_rpc.dispatch_exn Protocol.Join_room.rpc conn id
      in
      (id, updates)
  in
  match%map
    Rpc.Rpc.dispatch_exn Protocol.Start_playing.rpc conn Sit_anywhere
  with
  | Error (`Not_logged_in | `Not_in_a_room) -> assert false
  | Error ((`Game_already_started | `Seat_occupied) as error) ->
    raise_s [%message
      "Joined a room that didn't want new players"
        (error : Protocol.Start_playing.error)
    ]
  | Error `You're_already_playing
  | Ok (_ : Lobby.Room.Seat.t) -> updates

let run ~server ~config ~username ~room_choice ~auto_ready ~f =
  Rpc.Connection.with_client
    ~host:(Host_and_port.host server)
    ~port:(Host_and_port.port server)
    (fun conn ->
      let%bind updates = start_playing ~conn ~username ~room_choice in
      let state = State.create () in
      let ready_if_auto () =
        if auto_ready then (
          try_set_ready_on_conn ~conn
        ) else (
          Deferred.unit
        )
      in
      let handle_update : Protocol.Game_update.t -> unit =
        function
        | Broadcast (Round_over _) ->
          State.clear state;
          don't_wait_for (ready_if_auto ())
        | Broadcast (Exec exec) ->
          let fills = Exec.fills exec in
          if Username.equal username exec.order.owner then (
            let order_state =
              Hashtbl.find_exn state.orders exec.order.id
            in
            State.ack state ~order_state;
            List.iter fills ~f:(fun fill ->
                State.fill state ~order_state ~size:fill.size
              )
          );
          List.iter fills ~f:(fun fill ->
              Option.iter (Hashtbl.find state.orders fill.id)
                ~f:(fun order_state ->
                    State.fill state ~order_state ~size:fill.size
                  )
            )
        | Broadcast (Out order) ->
          State.out state ~id:order.id
        | Hand hand ->
          state.hand <- Some hand
        | _ -> ()
      in
      let updates = Pipe.map updates ~f:(fun u -> handle_update u; u) in
      let%bind () = ready_if_auto () in
      f { username; conn; updates; state } ~config)
  >>| Or_error.of_exn_result

let make_command ~summary ~config_param ~username_stem
    ?(auto_ready=false)
    f =
  let open Command.Let_syntax in
  Command.async_or_error'
    ~summary
    [%map_open
      let server =
        flag "-server" (required (Arg_type.create Host_and_port.of_string))
          ~doc:"HOST:PORT where to connect"
      and log_level =
        flag "-log-level" (optional_with_default `Info Log.Level.arg)
          ~doc:"L Debug, Info, or Error"
      and which =
        flag "-which" (optional int)
          ~doc:"N modulate username"
      and config = config_param
      and room_choice = Room_choice.param
      in
      fun () ->
        let username =
          username_stem ^ Option.value_map which ~default:"" ~f:Int.to_string
          |> Username.of_string
        in
        Log.Global.set_level log_level;
        Log.Global.sexp ~level:`Debug [%message
          "started"
            (username : Username.t)
        ];
        run ~server ~config ~username ~room_choice
          ~auto_ready
          ~f
    ]
