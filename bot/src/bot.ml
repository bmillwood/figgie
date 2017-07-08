open Core
open Async

open Figgie
open Market

type t =
  { username : Username.t
  ; conn     : Rpc.Connection.t
  ; updates  : Protocol.Game_update.t Pipe.Reader.t
  ; state    : State.t
  }

let username t = t.username
let updates  t = t.updates

let unacked_orders t = State.unacked_orders t.state

let open_orders t = State.open_orders t.state

let hand_if_no_fills t = State.hand_if_no_fills t.state

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

let players t = State.players t.state

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
  match%map
    Rpc.Rpc.dispatch_exn Protocol.Get_update.rpc t.conn thing_to_get
  with
  | Error (`Not_logged_in | `Not_in_a_room | `You're_not_playing) -> assert false
  | Error `Game_not_in_progress
  | Ok () -> ()

let try_set_ready_on_conn ~conn =
  Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc conn true
  |> Deferred.ignore

let try_set_ready t = try_set_ready_on_conn ~conn:t.conn

let start_playing ~conn ~username ~room_choice ~where_to_sit =
  let%bind () =
    match%map
      Rpc.Rpc.dispatch_exn Protocol.Login.rpc conn
        { username; is_bot = true }
    with
    | Error (`Already_logged_in | `Invalid_username) -> assert false
    | Ok () -> ()
  in
  let%bind (_room_id, updates) =
    Room_choice.join room_choice ~conn ~my_name:username
  in
  match%map
    Rpc.Rpc.dispatch_exn Protocol.Start_playing.rpc conn where_to_sit
  with
  | Error (`Not_logged_in | `Not_in_a_room) -> assert false
  | Error ((`Game_already_started | `Seat_occupied) as error) ->
    raise_s [%message
      "Joined a room that didn't want new players"
        (error : Protocol.Start_playing.error)
    ]
  | Error `You're_already_playing
  | Ok (_ : Lobby.Room.Seat.t) -> updates

let run ~conn ~config ~username ~room_choice ~where_to_sit ~auto_ready ~f =
  let%bind updates =
    start_playing ~conn ~username ~room_choice ~where_to_sit
  in
  let state = State.create ~username in
  let ready_if_auto () =
    if auto_ready then (
      try_set_ready_on_conn ~conn
    ) else (
      Deferred.unit
    )
  in
  let handle_update (update : Protocol.Game_update.t) =
    State.handle_update state update;
    match update with
    | Broadcast (Room_update (Round_over _)) ->
      don't_wait_for (ready_if_auto ())
    | _ -> ()
  in
  let updates = Pipe.map updates ~f:(fun u -> handle_update u; u) in
  let%bind () = ready_if_auto () in
  f { username; conn; updates; state } ~config

module Spec = struct
  type nonrec 'a t =
    { username_stem : string
    ; auto_ready    : bool
    ; run : t -> config:'a -> unit Deferred.t
    }

  let create ~username_stem ?(auto_ready=false) run =
    { username_stem; auto_ready; run }

  let username_of_stem stem which =
    stem ^ Option.value_map which ~default:"" ~f:Int.to_string
    |> Username.of_string

  let to_command spec ~summary ~config_param =
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
          let username = username_of_stem spec.username_stem which in
          Log.Global.set_level log_level;
          Log.Global.sexp ~level:`Debug [%message
            "started"
              (username : Username.t)
          ];
          Rpc.Connection.with_client
            ~host:(Host_and_port.host server)
            ~port:(Host_and_port.port server)
            (fun conn ->
              run ~conn ~config ~username ~room_choice
                ~where_to_sit:Sit_anywhere
                ~auto_ready:spec.auto_ready
                ~f:spec.run
            )
          |> Deferred.Or_error.of_exn_result
      ]

  let run spec ~conn ~avoid_username ~config ~room_choice ~where_to_sit =
    let username =
      let rec go n =
        let username = username_of_stem spec.username_stem (Some n) in
        if avoid_username username then (
          go (n + 1)
        ) else (
          username
        )
      in
      go 1
    in
    run ~conn ~config ~username ~room_choice ~where_to_sit
      ~auto_ready:spec.auto_ready
      ~f:spec.run
end
