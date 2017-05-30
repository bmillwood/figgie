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
  type t =
    { mutable next_order_id : Order.Id.t
    ; unacked_orders        : Order.t Order.Id.Table.t
    }

  let create () =
    { next_order_id = Order.Id.zero
    ; unacked_orders = Order.Id.Table.create ()
    }

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

let unacked_orders t = Hashtbl.data t.state.unacked_orders

module Staged_order = struct
  type t = Order.t

  let create bot ~symbol ~dir ~price ~size : t =
    { owner = username bot
    ; id = State.new_order_id bot.state
    ; symbol; dir; price; size
    }

  let id (t : t) = t.id

  let send_exn t bot =
    let id = id t in
    if Hashtbl.mem bot.state.unacked_orders id then (
      return (Error `Duplicate_order_id)
    ) else (
      Hashtbl.set bot.state.unacked_orders ~key:id ~data:t;
      let%map r = Rpc.Rpc.dispatch_exn Protocol.Order.rpc bot.conn t in
      Result.iter_error r ~f:(fun _ ->
          Hashtbl.remove bot.state.unacked_orders id
        );
      r
    )
end

let cancel t id = Rpc.Rpc.dispatch_exn Protocol.Cancel.rpc t.conn id

let request_update_exn t thing_to_get =
  Rpc.Rpc.dispatch_exn Protocol.Get_update.rpc t.conn thing_to_get
  >>| Protocol.playing_exn

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
          Hashtbl.clear state.unacked_orders;
          don't_wait_for (ready_if_auto ())
        | Broadcast (Exec exec) ->
          if Username.equal username exec.order.owner then (
            Hashtbl.remove state.unacked_orders exec.order.id
          )
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
