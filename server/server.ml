open Core
open Async
module Rpc_kernel = Async_rpc_kernel

open Figgie

module Connection_state = struct
  module Status = struct
    type t =
      | Not_logged_in of { conn : Rpc.Connection.t }
      | Logged_in of
        { conn : Rpc.Connection.t
        ; username : Username.t
        ; room : Room_manager.t option
        }
  end
  open Status

  type t = Status.t ref

  let create ~conn =
    ref (Not_logged_in { conn })
end

type t =
  { lobby_updates : Protocol.Lobby_update.t Updates_manager.t
  ; rooms : Room_manager.t Lobby.Room.Id.Table.t
  ; others : int Username.Map.t
  ; game_config : Game.Config.t
  ; chat_enabled : bool
  ; local_transport : Local_transport.t
  }

let lobby_snapshot t : Lobby.t =
  { rooms =
      Hashtbl.fold t.rooms ~init:Lobby.Room.Id.Map.empty
        ~f:(fun ~key ~data acc ->
            Map.add acc ~key ~data:(Room_manager.room_snapshot data)
          )
  ; others = t.others
  }

let new_room t ~room_id =
  if Hashtbl.mem t.rooms room_id then (
    Error `Room_already_exists
  ) else if not (Lobby.Room.Id.is_valid room_id) then (
    Error `Invalid_room_name
  ) else (
    let new_room =
      Room_manager.create ~id:room_id
        ~game_config:t.game_config
        ~lobby_updates:t.lobby_updates
    in
    Hashtbl.add_exn t.rooms ~key:room_id ~data:new_room;
    Updates_manager.broadcast t.lobby_updates
      (Lobby_update (New_empty_room { room_id }));
    Ok ()
  )

let new_room_exn t ~room_id =
  match new_room t ~room_id with
  | Error error ->
      raise_s [%message
        "Room creation failed" (error : Protocol.Create_room.error)
      ]
  | Ok () -> ()

let create ~game_config ~chat_enabled ~rooms =
  let t =
    { lobby_updates = Updates_manager.create ()
    ; rooms = Lobby.Room.Id.Table.create ()
    ; others = Username.Map.empty
    ; game_config
    ; chat_enabled
    ; local_transport = Local_transport.create ()
    }
  in
  List.iter (List.dedup_and_sort ~compare:Lobby.Room.Id.compare rooms)
    ~f:(fun room_id ->
      new_room_exn t ~room_id
  );
  t

let player_disconnected t ~(connection_state : Connection_state.t) =
  match !connection_state with
  | Not_logged_in _ -> ()
  | Logged_in { conn = _; username; room } ->
    match room with
    | None ->
      Updates_manager.broadcast t.lobby_updates
        (Lobby_update (Lobby_update { username; event = Disconnected }))
    | Some room ->
      Room_manager.player_disconnected room ~username

let implementations t =
  let from_lobby =
    [ Rpc.Rpc.implement Protocol.Login.rpc
        (fun (state : Connection_state.t) username ->
          match !state with
          | Not_logged_in { conn } ->
            if Username.is_valid username then (
              state := Logged_in { conn; username; room = None };
              Updates_manager.broadcast t.lobby_updates
                (Lobby_update (Lobby_update { username; event = Connected }));
              return (Ok ())
            ) else (
              return (Error `Invalid_username)
            )
          | Logged_in _ ->
            return (Error `Already_logged_in))
    ; Rpc.Pipe_rpc.implement Protocol.Get_lobby_updates.rpc
        (fun (state : Connection_state.t) () ->
          (* no reason in principle why we couldn't allow not-logged-in
             users to get lobby updates, it just means writing code *)
          return begin match !state with
          | Not_logged_in _ -> Error `Not_logged_in
          | Logged_in { username; _ } -> Ok username
          end
          >>=? fun username ->
          let (updates_r, updates_w) = Pipe.create () in
          Updates_manager.subscribe t.lobby_updates
            ~username ~updates:updates_w;
          Pipe.write_without_pushback updates_w
            (Lobby_snapshot (lobby_snapshot t));
          return (Ok updates_r))
    ; Rpc.Rpc.implement Protocol.Create_room.rpc
        (fun _state room_id ->
          return (new_room t ~room_id)
        )
    ; Rpc.Pipe_rpc.implement Protocol.Join_room.rpc
        (fun (state : Connection_state.t) room_id ->
          return begin match !state with
          | Not_logged_in _ -> Error `Not_logged_in
          | Logged_in { room = Some _; _ } -> Error `Already_in_a_room
          | Logged_in { room = None; username; conn } -> Ok (username, conn)
          end
          >>=? fun (username, conn) ->
          begin match Hashtbl.find t.rooms room_id with
          | None -> return (Error `No_such_room)
          | Some room -> return (Ok room)
          end
          >>=? fun room ->
          let updates_r = Room_manager.player_join room ~username in
          state := Logged_in { username; conn; room = Some room };
          return (Ok updates_r)
      )
    ; Rpc.Rpc.implement Protocol.Request_bot.rpc
        (fun (_state : Connection_state.t) { type_; room; seat } ->
          Local_transport.connect t.local_transport
          >>| ok_exn
          >>= fun conn ->
          let avoid_username =
            let lobby = lobby_snapshot t in
            fun username -> Lobby.has_user lobby ~username
          in
          let room_choice  : Botlib.Room_choice.t = Named room in
          let where_to_sit : Protocol.Start_playing.query = Sit_in seat in
          let run_bot =
            match type_ with
            | Chaos -> Botlib.Chaos.run
            | Count -> Botlib.Count.run
            | Sell -> Botlib.Sell.run
          in
          don't_wait_for (
            run_bot ~conn ~avoid_username ~room_choice ~where_to_sit
          );
          return (Ok ())
      )
    ; Rpc.Rpc.implement Protocol.Delete_room.rpc
        (fun (_state : Connection_state.t) room_id ->
          match Hashtbl.find t.rooms room_id with
          | None -> return (Error `No_such_room)
          | Some rm ->
            let room = Room_manager.room_snapshot rm in
            if not (Lobby.Room.can_delete room) then (
              return (Error `Room_in_use)
            ) else (
              Hashtbl.remove t.rooms room_id;
              Updates_manager.broadcast t.lobby_updates
                (Lobby_update (Room_closed { room_id }));
              return (Ok ())
            )
      )
    ; Rpc.Rpc.implement Protocol.Chat.rpc
        (fun (state : Connection_state.t) msg ->
          if not t.chat_enabled then (
            return (Error `Chat_disabled)
          ) else (
            match !state with
            | Not_logged_in _ -> return (Error `Not_logged_in)
            | Logged_in { room = None; username; conn = _ } ->
              Updates_manager.broadcast t.lobby_updates
                (Chat (username, msg));
              return (Ok ())
            | Logged_in { room = Some room; username; conn = _ } ->
              Room_manager.chat room ~username msg;
              return (Ok ())
          )
      )
    ]
  in
  let from_room =
    List.map Room_manager.rpc_implementations ~f:(fun impl ->
        Rpc.Implementation.lift impl ~f:(fun (state : Connection_state.t) ->
            match !state with
            | Not_logged_in _ -> Error `Not_logged_in
            | Logged_in { room = None; _ } -> Error `Not_in_a_room
            | Logged_in { room = Some room; username; conn = _ } ->
              Ok (room, username)
          )
      )
  in
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:(from_lobby @ from_room)

let serve t ~tcp_port ~web_port =
  let implementations = implementations t in
  let initial_connection_state addr conn =
    let state = Connection_state.create ~conn in
    Deferred.upon (Rpc.Connection.close_reason conn ~on_close:`started)
      (fun reason ->
        player_disconnected t ~connection_state:state;
        Log.Global.sexp ~level:`Info [%message
          "disconnected"
            (addr : Socket.Address.Inet.t option)
            (reason : Info.t)
          ]
      );
    state
  in
  Local_transport.listen t.local_transport ~f:(fun transport ->
      Rpc_kernel.Rpc.Connection.server_with_close transport
        ~implementations
        ~on_handshake_error:`Raise
        ~connection_state:(fun conn -> initial_connection_state None conn)
    );
  let%bind _server =
    Rpc.Connection.serve
      ~initial_connection_state:(fun addr conn ->
          initial_connection_state (Some addr) conn
        )
      ~implementations
      ~where_to_listen:(Tcp.on_port tcp_port)
      ()
  and () =
    Web_transport.serve ~port:web_port
      ~f:(fun addr transport ->
        Rpc_kernel.Rpc.Connection.server_with_close transport
          ~implementations
          ~on_handshake_error:`Raise
          ~connection_state:(fun conn ->
              initial_connection_state (Some addr) conn
            )
      )
    >>| function
    | Ok () | Error () -> ()
  in
  Deferred.never ()

let command =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"Figgie server"
    [%map_open
      let tcp_port =
        flag "-tcp-port"
          (optional_with_default Protocol.default_async_rpc_port int)
          ~doc:"N port to listen on for Async-RPC clients"
      and web_port =
        flag "-web-port"
          (optional_with_default Protocol.default_websocket_port int)
          ~doc:"N port to listen on for websocket clients"
      and log_level =
        flag "-log-level" (optional_with_default `Info Log.Level.arg)
          ~doc:"LEVEL Error, Info, or Debug"
      and game_config = Game.Config.arg
      and chat_enabled =
        flag "-enable-chat" (required bool)
          ~doc:"BOOL enable player-player comnunications"
      and rooms =
        flag "-room" (listed (Arg_type.create Lobby.Room.Id.of_string))
          ~doc:"ROOM-ID create this room at startup"
      in
      fun () ->
        Log.Global.set_level log_level;
        Random.self_init ();
        let t = create ~game_config ~chat_enabled ~rooms in
        serve t ~tcp_port ~web_port
        >>= never_returns
    ]
