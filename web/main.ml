open Core_kernel.Std
open Async_kernel
open Async_rpc_kernel
open Incr_dom
open Vdom

open Figgie
open Market

module Waiting = struct
  module Model = struct
    type t =
      { last_gold : Card.Suit.t option
      ; ready : Username.Set.t
      }

    let initial = { last_gold = None; ready = Username.Set.empty }
  end
  open Model

  let set_player_readiness t ~username ~is_ready =
    let apply = if is_ready then Set.add else Set.remove in
    { t with ready = apply t.ready username }
end

module Playing = struct
  module Model = struct
    type t = { clock : Countdown.Model.t option }

    let initial = { clock = None }
  end

  module Action = struct
    type t =
      | Set_clock of Time_ns.t sexp_opaque
      | Clock of Countdown.Action.t
    [@@deriving sexp_of]
  end
end

module In_room = struct
  module Model = struct
    module Game = struct
      type t =
        | Waiting of Waiting.Model.t
        | Playing of Playing.Model.t
    end

    type t =
      { room_id  : Lobby.Room.Id.t
      ; room     : Lobby.Room.t
      ; users    : Lobby.User.t Username.Map.t
      ; exchange : Exchange.Model.t
      ; game     : Game.t
      }
  end
  open Model

  let update_round_if_playing t ~f =
    match t.game with
    | Playing round -> { t with game = Playing (f round) }
    | Waiting _ -> t

  let update_ready_if_waiting t ~f =
    match t.game with
    | Playing _ -> t
    | Waiting wait -> { t with game = Waiting (f wait) }

  let modify_user (t : Model.t) ~username ~f =
    let users = Map.change t.users username ~f:(Option.map ~f) in
    { t with users }

  module Action = struct
    type t =
      | Game_update of Protocol.Game_update.t
      | Playing of Playing.Action.t
      | I'm_ready of bool
      | Exchange of Exchange.Action.t
    [@@deriving sexp_of]
  end
end

module Logged_in = struct
  module Model = struct
    module Where = struct
      type t =
        | Lobby of
          { lobby : Lobby.t
          ; updates : Protocol.Lobby_update.t Pipe.Reader.t
          }
        | In_room of In_room.Model.t
    end

    type t =
      { my_name  : Username.t
      ; where    : Where.t
      }
  end

  let update_in_room (t : Model.t) ~f =
    match t.where with
    | Lobby _ -> t
    | In_room r -> { t with where = In_room (f r) }

  module Action = struct
    type t =
      | Lobby_update of Protocol.Lobby_update.t
      | Join_room of Lobby.Room.Id.t
      | In_room of In_room.Action.t
    [@@deriving sexp_of]
  end
end

module Connected = struct
  module Model = struct
    type t = {
      conn : Rpc.Connection.t;
      host_and_port : Host_and_port.t;
      login : Logged_in.Model.t option;
    }
  end

  module Action = struct
    type t =
      | Start_login of Username.t
      | Finish_login of
        { username : Username.t
        ; lobby_pipe : Protocol.Lobby_update.t Pipe.Reader.t
        }
      | Logged_in of Logged_in.Action.t
    [@@deriving sexp_of]
  end
end

module App = struct
  module Model = struct
    module Connection_state = struct
      type t =
        | Not_connected of Connection_error.t option
        | Connecting of Host_and_port.t
        | Connected of Connected.Model.t
    end

    module For_status_line = struct
      type t =
        { input_error : bool
        ; connectbox_prefill : string option
        }
    end

    type t =
      { messages : Chat.Model.t
      ; state : Connection_state.t
      ; for_status_line : For_status_line.t
      }

    let initial =
      { messages = Chat.Model.initial
      ; state = Not_connected None
      ; for_status_line =
        { input_error = false
        ; connectbox_prefill = Url_vars.prefill_connect_to
        }
      }

    let get_conn t =
      match t.state with
      | Connected conn -> Some conn
      | _ -> None

    let cutoff = phys_equal
  end
  open Model

  module Action = struct
    type t =
      | Add_message of Chat.Message.t
      | Send_chat of string
      | Scroll_chat of Scrolling.Action.t
      | Status_line of Status_line.Action.t
      | Finish_connecting of
        { host_and_port : Host_and_port.t
        ; conn : Rpc.Connection.t
        }
      | Connection_failed
      | Connection_lost
      | Connected of Connected.Action.t
    [@@deriving sexp_of]

    let should_log _ = false

    let logged_in lact = Connected (Logged_in lact)
    let in_room inact  = logged_in (In_room inact)
    let playing pact   = in_room (Playing pact)
    let clock cdact    = playing (Clock cdact)
  end

  module State = struct
    type t = { schedule : Action.t -> unit }
  end

  let apply_playing_action
      (t : Playing.Action.t)
      ~(schedule : Action.t -> _)
      (round : Playing.Model.t)
      : Playing.Model.t
    =
    match t with
    | Set_clock end_time ->
      let clock =
        Countdown.of_end_time
          ~schedule:(fun cdact -> schedule (Action.clock cdact))
          end_time
      in
      { clock = Some clock }
    | Clock cdact ->
      let clock =
        Option.map round.clock ~f:(fun c ->
          Countdown.Action.apply cdact
            ~schedule:(fun cdact -> schedule (Action.clock cdact))
            c)
      in
      { clock }

  let apply_in_room_action
      (t : In_room.Action.t)
      (in_room : In_room.Model.t)
      ~(schedule : Action.t -> _)
      ~conn ~my_name : In_room.Model.t
    =
    match t with
    | I'm_ready readiness ->
      In_room.update_ready_if_waiting in_room ~f:(fun wait ->
          don't_wait_for begin
            Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc conn readiness
            >>| function
            | Ok ()
            | Error `Game_already_in_progress
            | Error `You're_not_playing
            | Error `Not_logged_in
            | Error `Not_in_a_room -> ()
          end;
          Waiting.set_player_readiness wait
            ~username:my_name
            ~is_ready:readiness
        )
    | Playing pact ->
      In_room.update_round_if_playing in_room
        ~f:(apply_playing_action pact ~schedule)
    | Exchange exact ->
      let exchange =
        Exchange.apply_action exact in_room.exchange
          ~my_name ~conn
          ~add_message:(fun msg -> schedule (Add_message msg))
      in
      { in_room with exchange }
    | Game_update up ->
      let just_schedule act = schedule act; in_room in
      match up with
      | Room_snapshot room ->
        { in_room with users = Lobby.Room.users room }
      | Hand hand ->
        let users =
          Map.change in_room.users my_name
            ~f:(Option.map ~f:(fun user ->
              Lobby.User.set_hand_if_player user
                ~hand:(Partial_hand.create_known hand)
          ))
        in
        { in_room with users }
      | Market market ->
        let users =
          Map.map in_room.users ~f:(fun user ->
            match Lobby.User.role user with
            | Observer _ -> user
            | Player { score = _; hand } ->
              let hand =
                Card.Hand.foldi market ~init:hand
                  ~f:(fun suit hand book ->
                    let size =
                      List.sum (module Size) book.sell ~f:(fun order ->
                          if Username.equal
                              order.owner
                              (Lobby.User.username user)
                          then order.size
                          else Size.zero
                        )
                    in
                    Partial_hand.selling hand ~suit ~size
                  )
              in
              Lobby.User.set_hand_if_player user ~hand
          )
        in
        let exchange = Exchange.set_market in_room.exchange ~market in
        { in_room with users; exchange }
      | Broadcast (Exec (order, exec)) ->
        let { Order.owner; id; dir; _ } = order in
        let trades =
          List.map (Exec.fills exec) ~f:(fun filled_order ->
              ({ filled_order with owner; id; dir }, filled_order.owner)
            )
        in
        let users =
          Map.map in_room.users ~f:(fun user ->
            match Lobby.User.role user with
            | Observer _ -> user
            | Player { score = _; hand } ->
              let new_hand =
                List.fold trades
                  ~init:hand
                  ~f:(fun hand (trade, with_) ->
                    let traded dir =
                      Partial_hand.traded
                        hand ~suit:trade.symbol ~size:trade.size ~dir
                    in
                    let username = Lobby.User.username user in
                    if Username.equal username trade.owner
                    then traded trade.dir
                    else if Username.equal username with_
                    then traded (Dir.other order.dir)
                    else hand)
              in
              Lobby.User.set_hand_if_player user ~hand:new_hand
          )
        in
        let exchange =
          List.fold trades ~init:in_room.exchange
            ~f:(fun exch (traded, with_) ->
                Exchange.add_trade exch ~traded ~with_
              )
        in
        don't_wait_for begin
          let%map () =
            Rpc.Rpc.dispatch_exn Protocol.Get_update.rpc conn Market
            >>| Protocol.playing_exn
          and () =
            Rpc.Rpc.dispatch_exn Protocol.Get_update.rpc conn Hand
            >>| Protocol.playing_exn
          in
          ()
        end;
        { in_room with users; exchange }
      | Broadcast (Room_update { username; event }) ->
        schedule (Add_message (Player_room_event
          { username; room_id = None; event }
        ));
        begin match event with
        | Joined ->
          let users =
            Map.update in_room.users username
              ~f:(function
                | None ->
                  { username
                  ; role = Observer { is_omniscient = false }
                  ; is_connected = true
                  }
                | Some user ->
                  { user with is_connected = true }
                )
          in
          { in_room with users }
        | Observer_started_playing { in_seat = _ } ->
          In_room.modify_user in_room ~username ~f:(fun user ->
              let score = Price.zero in
              let hand = Partial_hand.empty in
              { user with role = Player { score; hand } }
            )
        | Disconnected ->
          In_room.modify_user in_room ~username ~f:(fun user ->
              { user with is_connected = false }
            )
        | Observer_became_omniscient ->
          In_room.modify_user in_room ~username ~f:(fun user ->
              { user with role = Observer { is_omniscient = true } }
            )
        | Player_score score ->
          In_room.modify_user in_room ~username ~f:(fun user ->
              match user.role with
              | Observer _ -> user
              | Player { score = _; hand } ->
                { user with role = Player { score; hand } }
            )
        | Player_hand hand ->
          In_room.modify_user in_room ~username ~f:(fun user ->
              match user.role with
              | Observer _ -> user
              | Player { score; hand = _ } ->
                { user with role = Player { score; hand } }
            )
        end
      | Broadcast New_round ->
        schedule (Add_message New_round);
        don't_wait_for begin
          (* Sample the time *before* we send the RPC. Then the game end
             time we get is actually roughly "last time we can expect to
             send an RPC and have it arrive before the game ends". *)
          let current_time = Time_ns.now () in
          Rpc.Rpc.dispatch_exn Protocol.Time_remaining.rpc conn ()
          >>| function
          | Error #Protocol.not_playing -> ()
          | Ok remaining ->
            let end_time = Time_ns.add current_time remaining in
            schedule (Action.playing (Set_clock end_time))
        end;
        let users =
          Map.map in_room.users ~f:(fun user ->
              match user.role with
              | Observer _ -> user
              | Player { score; hand = _ } ->
                let hand =
                  Partial_hand.create_unknown Params.num_cards_per_hand
                in
                { user with role = Player { score; hand } }
            )
        in
        let exchange = Exchange.Model.empty in
        let playing = Playing.Model.initial in
        { in_room with users; exchange; game = Playing playing }
      | Broadcast (Round_over results) ->
        schedule (Add_message (Round_over results));
        let waiting : Waiting.Model.t =
          { last_gold = Some results.gold
          ; ready = Username.Set.empty
          }
        in
        { in_room with game = Waiting waiting }
      | Broadcast (Chat (username, msg)) ->
        let is_me = Username.equal username my_name in
        just_schedule (Add_message (Chat { username; is_me; msg }))
      | Broadcast (Out _) ->
        don't_wait_for begin
          Rpc.Rpc.dispatch_exn Protocol.Get_update.rpc conn Market
          >>| Protocol.playing_exn
        end;
        in_room
      | Broadcast (Player_ready { who; is_ready }) ->
        In_room.update_ready_if_waiting in_room ~f:(fun wait ->
          Waiting.set_player_readiness wait ~username:who ~is_ready)

  let apply_logged_in_action
      (t : Logged_in.Action.t)
      ~(schedule : Action.t -> _) ~conn
      (login : Logged_in.Model.t) : Logged_in.Model.t
    =
    match t with
    | Lobby_update up ->
      begin match login.where with
      | Lobby { lobby; updates } ->
        begin match up with
        | Chat (username, msg) ->
          let is_me = Username.equal username login.my_name in
          schedule (Add_message (Chat { username; is_me; msg }));
          login
        | Lobby_snapshot new_lobby ->
          { login with where = Lobby { lobby = new_lobby; updates } }
        | Lobby_update up ->
          begin match up with
          | Room_update { room_id; update = { username; event } } ->
            schedule (Add_message
              (Player_room_event { username; room_id = Some room_id; event })
            )
          | Lobby_update { username; event } ->
            schedule (Add_message
              (Player_lobby_event { username; event })
            )
          | _ -> ()
          end;
          let new_lobby = Lobby.Update.apply up lobby in
          { login with where = Lobby { lobby = new_lobby; updates } }
        end
      | _ -> login
      end
    | Join_room id ->
      begin match login.where with
      | In_room _ -> login
      | Lobby { lobby; updates } ->
        Pipe.close_read updates;
        don't_wait_for begin
          Rpc.Pipe_rpc.dispatch_exn Protocol.Join_room.rpc conn id
          >>= fun (pipe, _metadata) ->
          schedule (Add_message (Joined_room id));
          Pipe.iter_without_pushback pipe ~f:(fun update ->
              schedule (Action.in_room (Game_update update))
            )
          |> don't_wait_for;
          Rpc.Rpc.dispatch_exn Protocol.Start_playing.rpc conn Sit_anywhere
          >>| begin function
          | Error (`Not_logged_in | `Not_in_a_room) -> assert false
          | Error (`Game_already_started | `Seat_occupied) -> assert false
          | Error `You're_already_playing | Ok (_ : Lobby.Room.Seat.t) -> ()
          end
        end;
        let in_room : In_room.Model.t =
          let room =
            Option.value ~default:Lobby.Room.empty
              (Map.find lobby.rooms id)
          in
          { room_id = id
          ; room
          ; users = Lobby.Room.users room
          ; exchange = Exchange.Model.empty
          ; game = Waiting Waiting.Model.initial
          }
        in
        { login with where = In_room in_room }
      end
    | In_room iract ->
      begin match login.where with
      | In_room room ->
        let room =
          apply_in_room_action iract room
            ~schedule ~conn ~my_name:login.my_name
        in
        { login with where = In_room room }
      | Lobby _ -> login
      end

  let apply_connected_action
      (t : Connected.Action.t)
      ~(schedule : Action.t -> _)
      (conn : Connected.Model.t) : Connected.Model.t
    =
    match t with
    | Start_login username ->
      don't_wait_for begin
        Rpc.Rpc.dispatch_exn Protocol.Login.rpc conn.conn username
        >>= function
        | Error (`Already_logged_in | `Invalid_username) -> Deferred.unit
        | Ok () ->
          Rpc.Pipe_rpc.dispatch_exn Protocol.Get_lobby_updates.rpc
            conn.conn ()
          >>= fun (pipe, _pipe_metadata) ->
          schedule (Connected (Finish_login { username; lobby_pipe = pipe }));
          Pipe.iter_without_pushback pipe
            ~f:(fun update -> schedule (Action.logged_in (Lobby_update update)))
      end;
      conn
    | Finish_login { username; lobby_pipe } ->
      let logged_in =
        { Logged_in.Model.my_name = username
        ; where = Lobby { lobby = Lobby.empty; updates = lobby_pipe }
        }
      in
      { conn with login = Some logged_in }
    | Logged_in lact ->
      begin match conn.login with
      | None -> conn
      | Some logged_in ->
        let logged_in =
          apply_logged_in_action lact ~schedule ~conn:conn.conn logged_in
        in
        { conn with login = Some logged_in }
      end

  let apply_action (action : Action.t) (model : Model.t) (state : State.t) =
    match action with
    | Add_message msg ->
      { model with messages = Chat.Model.add_message model.messages msg }
    | Send_chat msg ->
      Option.iter (get_conn model) ~f:(fun { conn; _ } ->
        don't_wait_for begin
          Rpc.Rpc.dispatch_exn Protocol.Chat.rpc conn msg
          >>| function
          | Error `Chat_disabled ->
            state.schedule (Action.Add_message (Chat_failed `Chat_disabled))
          | Error `Not_logged_in ->
            state.schedule (Action.Add_message (Chat_failed `Not_logged_in))
          | Ok () -> ()
        end
      );
      model
    | Scroll_chat act ->
      { model with messages = Chat.apply_scrolling_action model.messages act }
    | Status_line Input_error ->
      { model with for_status_line =
        { model.for_status_line with input_error = true }
      }
    | Status_line Input_ok ->
      { model with for_status_line =
        { model.for_status_line with input_error = false }
      }
    | Status_line (Log_in username) ->
      state.schedule (Connected (Start_login username));
      { model with for_status_line =
        { model.for_status_line with input_error = false }
      }
    | Status_line (Start_connecting_to host_and_port) ->
      let host, port = Host_and_port.tuple host_and_port in
      don't_wait_for begin
        Async_js.Rpc.Connection.client
          ~address:(Host_and_port.create ~host ~port)
          ()
        >>| function
        | Error _error ->
          state.schedule Connection_failed
        | Ok conn ->
          state.schedule (Finish_connecting { host_and_port; conn });
          don't_wait_for (
            Rpc.Connection.close_finished conn
            >>| fun () ->
            state.schedule Connection_lost
          )
      end;
      { model with
        state = Connecting host_and_port
      ; for_status_line =
        { input_error = false
        ; connectbox_prefill = Some (Host_and_port.to_string host_and_port)
        }
      }
    | Finish_connecting { host_and_port; conn } ->
      state.schedule (Add_message (Connected_to_server host_and_port));
      { model with state = Connected { host_and_port; conn; login = None } }
    | Connection_lost ->
      state.schedule (Add_message Disconnected_from_server);
      { model with state = Not_connected (Some Connection_lost) }
    | Connection_failed ->
      { model with state = Not_connected (Some Failed_to_connect) }
    | Connected cact ->
      begin match model.state with
      | Connected conn ->
        let conn =
          apply_connected_action cact ~schedule:state.schedule conn
        in
        { model with state = Connected conn }
      | _ -> model
      end

  let chat_view (model : Model.t) ~(inject : Action.t -> _) =
    let chat_inject : Chat.Action.t -> _ = function
      | Send_chat msg ->
        inject (Send_chat msg)
      | Scroll_chat act ->
        inject (Scroll_chat act)
    in
    Chat.view model.messages
      ~is_connected:(Option.is_some (get_conn model))
      ~inject:chat_inject

  let status_line (model : Model.t) : Status_line.Model.t =
    match model.state with
    | Not_connected conn_error ->
      Not_connected
        { conn_error
        ; input_error = model.for_status_line.input_error
        ; connectbox_prefill = model.for_status_line.connectbox_prefill
        }
    | Connecting hp -> Connecting hp
    | Connected { host_and_port; login; _ } ->
      begin match login with
      | None -> Connected host_and_port
      | Some login ->
        let room_id, clock =
          match login.where with
          | In_room { room_id; game; _ } ->
            ( Some room_id
            , match game with
              | Playing { clock } -> clock
              | Waiting _ -> None
            )
          | Lobby _ -> None, None
        in
        Logged_in
          { connected_to = host_and_port
          ; username = login.my_name
          ; room_id
          ; clock
          }
      end

  let view (incr_model : Model.t Incr.t) ~(inject : Action.t -> _) =
    let open Incr.Let_syntax in
    let%map model = incr_model in
    let view bits_in_between =
      Node.body
        [ Hotkeys.Global.handler ]
        [ Node.div [Attr.id "container"] (
            [ [ Status_line.view
                  (status_line model)
                  ~inject:(fun act -> inject (Status_line act))
              ]
            ; bits_in_between 
            ; [chat_view model ~inject]
            ] |> List.concat
          )
        ]
    in
    match get_conn model with
    | None | Some { login = None; _ } -> view []
    | Some { login = Some login; conn; host_and_port = _ } ->
      let my_name = login.my_name in
      let exchange_inject act = inject (Action.in_room (Exchange act)) in
      begin match login.where with
      | Lobby { lobby; updates = _ } ->
        [ Lobby_view.view lobby
            ~my_name
            ~inject:(function
              | Join_room id ->
                inject (Action.logged_in (Join_room id))
              | Delete_room id ->
                don't_wait_for begin
                  let open Async_kernel in
                  Rpc.Rpc.dispatch_exn Protocol.Delete_room.rpc conn id
                  >>| function
                  | Error (`No_such_room | `Room_in_use) -> ()
                  | Ok () -> ()
                end;
                Event.Ignore
            )
        ] |> view
      | In_room { exchange; game; users; _ } ->
        [ Exchange.view exchange ~my_name
            ~players:(Set.of_map_keys users)
            ~inject:exchange_inject
        ; match game with
          | Playing _ ->
            User_info.playing ~users ~my_name
          | Waiting { last_gold; ready } ->
            User_info.waiting
              ~inject_I'm_ready:(fun readiness ->
                inject (Action.in_room (I'm_ready readiness)))
              ~users ~my_name ~last_gold
              ~who_is_ready:ready
        ] |> view
      end

  let on_startup ~schedule _model =
    Option.iter Url_vars.auto_connect_to
      ~f:(fun hp -> schedule (Action.Status_line (Start_connecting_to hp)));
    return { State.schedule }

  let on_display ~(old : Model.t) (new_ : Model.t) (state : State.t) =
    begin match get_conn new_ with
    | Some { login = Some { where = In_room r; _ }; _ } ->
      Exchange.on_display r.exchange
        ~schedule:(fun exact ->
            state.schedule (Action.in_room (Exchange exact))
          )
    | _ -> ()
    end;
    Status_line.on_display ~old:(status_line old) (status_line new_);
    Chat.on_display
      ~old:old.messages
      new_.messages
      ~schedule_scroll:(fun act -> state.schedule (Scroll_chat act))

  let update_visibility model = model
end

let () =
  Start_app.simple
    ~initial_model:App.Model.initial
    (module App)
