open Core
open Async

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
      | Error `Already_logged_in -> assert false
      | Ok () ->
        begin match room_choice with
        | First_available ->
          Rpc.Pipe_rpc.dispatch Protocol.Get_lobby_updates.rpc conn ()
          >>| ok_exn
          >>= begin function
          | Error `Not_logged_in -> assert false
          | Ok (lobby_updates, _metadata) ->
            let finished id =
              Pipe.close_read lobby_updates;
              return (`Finished id)
            in
            Deferred.repeat_until_finished false (fun waiting_for_rooms ->
              Pipe.read lobby_updates
              >>= function
              | `Eof -> failwith "Server hung up on us"
              | `Ok (Lobby_update (Snapshot lobby)) ->
                begin match
                  List.find (Map.to_alist lobby.rooms) ~f:(fun (_id, room) ->
                    not (Lobby.Room.is_full room))
                with
                | Some (id, room) ->
                  Log.Global.sexp ~level:`Debug [%message
                    "doesn't look full, joining"
                      (id : Lobby.Room.Id.t) (room : Lobby.Room.t)
                  ];
                  finished id
                | None ->
                  Log.Global.sexp ~level:`Debug [%message
                    "couldn't see an empty room, waiting"
                  ];
                  return (`Repeat true)
                end
              | `Ok (Lobby_update (New_room { id; room }))
                when waiting_for_rooms && not (Lobby.Room.is_full room) ->
                finished id
              | _ -> return (`Repeat waiting_for_rooms)
            )
          end
        | Named id -> return id
        end
        >>= fun room_id ->
        Log.Global.sexp ~level:`Debug [%message
          "joining room"
            (room_id : Lobby.Room.Id.t)
        ];
        Rpc.Pipe_rpc.dispatch Protocol.Join_room.rpc conn room_id
        >>| ok_exn
        >>= function
        | Error (`Not_logged_in | `Already_in_a_room) -> assert false
        | Error error ->
          (* should retry if room is full and room_choice = First_available *)
          raise_s [%message
            "joining room failed"
              (error : Protocol.Join_room.error)
          ]
        | Ok (updates, _metadata) ->
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
