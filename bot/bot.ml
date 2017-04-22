open Core
open Async

open Figgie

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

type 'a t =
  { config : 'a
  ; username : Username.t
  ; conn : Rpc.Connection.t
  ; updates : Protocol.Game_update.t Pipe.Reader.t
  ; new_order_id : unit -> Market.Order.Id.t
  }

let run ~server ~config ~username ~(room_choice : Room_choice.t) ~f =
  Rpc.Connection.with_client
    ~host:(Host_and_port.host server)
    ~port:(Host_and_port.port server)
    (fun conn ->
      Rpc.Rpc.dispatch_exn Protocol.Login.rpc conn username
      >>= function
      | Error (`Already_logged_in | `Invalid_username) -> assert false
      | Ok () ->
        begin match room_choice with
        | First_available ->
          Rpc.Pipe_rpc.dispatch Protocol.Get_lobby_updates.rpc conn ()
          >>| ok_exn
          >>= begin function
          | Error `Not_logged_in -> assert false
          | Ok (lobby_updates, _metadata) ->
            let can_join room =
              Lobby.Room.has_player room ~username
              || not (Lobby.Room.is_full room)
            in
            let try_to_join id =
              Log.Global.sexp ~level:`Debug [%message
                "joining room"
                  (id : Lobby.Room.Id.t)
              ];
              Rpc.Pipe_rpc.dispatch Protocol.Join_room.rpc conn id
              >>| ok_exn
              >>| function
              | Ok (updates, _pipe_metadata) ->
                Pipe.close_read lobby_updates;
                `Finished updates
              | Error (`Already_in_a_room | `Not_logged_in) -> assert false
              | Error
                ( `No_such_room
                | `Game_already_started
                | `Game_is_full
                ) -> `Repeat ()
            in
            Deferred.repeat_until_finished () (fun () ->
              Pipe.read lobby_updates
              >>= function
              | `Eof -> failwith "Server hung up on us"
              | `Ok (Lobby_update (Snapshot lobby)) ->
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
              | `Ok (Lobby_update (New_room { id; room }))
                when can_join room ->
                try_to_join id
              | _ -> return (`Repeat ())
            )
          end
        | Named id ->
          Rpc.Pipe_rpc.dispatch_exn Protocol.Join_room.rpc conn id
          >>| fun (updates, _metadata) ->
          updates
        end
        >>= fun updates ->
        Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc conn true
        >>= function
        | Error (`Not_logged_in | `Not_in_a_room | `Already_playing) ->
          assert false
        | Ok () ->
          let new_order_id =
            let r = ref Market.Order.Id.zero in
            fun () ->
              let id = !r in
              r := Market.Order.Id.next id;
              id
          in
          f { config; username; conn; updates; new_order_id })
  >>| Or_error.of_exn_result

let make_command ~summary ~config_param ~username_stem ~f =
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
        run ~server ~config ~username ~room_choice ~f
    ]
