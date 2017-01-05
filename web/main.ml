open Core_kernel.Std
open Async_kernel.Std
open Async_rpc_kernel.Std
open Incr_dom.Std
open Vdom

open Market

module Waiting = struct
  module Model = struct
    type t = { ready : Username.Set.t }
  end

  let set_player_readiness (t : Model.t) ~username ~is_ready =
    let apply = if is_ready then Set.add else Set.remove in
    { Model.ready = apply t.ready username }
end

module Playing = struct
  module Model = struct
    type t = {
      my_hand       : Size.t Card.Hand.t;
      other_hands   : Partial_hand.t Username.Map.t;
      market        : Book.t;
      trades        : (Order.t * Cpty.t) Fqueue.t;
      trades_scroll : Scrolling.Model.t;
      next_order    : Order.Id.t;
      clock         : Countdown.Model.t option;
    }
  end

  module Cancel_scope = struct
    type t =
      | All
      | By_id of Order.Id.t
      | By_symbol_side of { symbol : Symbol.t; dir : Dir.t }
      [@@deriving sexp_of]
  end

  module Action = struct
    type t =
      | Market of Book.t
      | Hand of Size.t Card.Hand.t
      | Trade of Order.t * Cpty.t
      | Scroll_trades of Scrolling.Action.t
      | Score of Price.t
      | Send_order of {
          symbol : Card.Suit.t;
          dir    : Dir.t;
          price  : Price.t;
        }
      | Send_cancel of Cancel_scope.t
      | Set_clock of Time_ns.t sexp_opaque
      | Clock of Countdown.Action.t
      [@@deriving sexp_of]
  end
end

module Game = struct
  module Model = struct
    type t =
      | Waiting of Waiting.Model.t
      | Playing of Playing.Model.t
  end

  module Action = struct
    type t =
      | I'm_ready of bool
      | Start_playing
      | Playing of Playing.Action.t
      | Round_over
      [@@deriving sexp_of]
  end
end

module Logged_in = struct
  module Model = struct
    type t = {
      me     : Player.Persistent.t;
      others : Player.Persistent.t Username.Map.t;
      game   : Game.Model.t;
    }
  end

  let new_player (t : Model.t) ~username =
    let n =
      let rec loop n =
        if Map.exists t.others
          ~f:(fun other -> Player.Style.equal other.style (Them n))
        then loop (n + 1)
        else n
      in
      loop 1
    in
    { Player.Persistent.username; style = Them n; score = Price.zero }

  let update_round_if_playing (t : Model.t) ~f =
    match t.game with
    | Playing round -> { t with game = Playing (f round) }
    | Waiting _ -> t

  let update_ready_if_waiting (t : Model.t) ~f =
    match t.game with
    | Playing _ -> t
    | Waiting wait -> { t with game = Waiting (f wait) }

  let update_player (t : Model.t) ~username ~f =
    if Username.equal t.me.username username
    then { t with me = f t.me }
    else begin
      let others =
        Map.update t.others username
          ~f:(fun maybe_existing ->
            Option.value maybe_existing ~default:(new_player t ~username)
            |> f)
      in
      { t with others }
    end

  let add_new_player (t : Model.t) ~username =
    update_player t ~username ~f:Fn.id

  let player (t : Model.t) ~username =
    if Username.equal t.me.username username
    then Some t.me
    else Map.find t.others username

  module Action = struct
    type t =
      | Game of Game.Action.t
      | Update of Protocol.Player_update.t
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
      | Finish_login of Username.t
      | Logged_in of Logged_in.Action.t
      [@@deriving sexp_of]
  end
end

module App = struct
  module Model = struct
    module Connection_state = struct
      type t =
        | Disconnected
        | Connecting of Host_and_port.t
        | Connected of Connected.Model.t
        | Connection_failed
    end

    type t =
      { messages : Chat.Model.t
      ; state : Connection_state.t
      }

    let initial =
      { messages = Chat.Model.initial
      ; state = Disconnected
      }

    let cutoff = phys_equal
  end
  open Model

  module Action = struct
    type t =
      | Add_message of Chat.Message.t
      | Scroll_chat of Scrolling.Action.t
      | Start_connecting of Host_and_port.t
      | Finish_connecting of
        { host_and_port : Host_and_port.t
        ; conn : Rpc.Connection.t
        }
      | Connection_failed
      | Connected of Connected.Action.t
      [@@deriving sexp_of]

    let should_log _ = false

    let connected cact = Connected cact
    let logged_in lact = connected (Logged_in lact)
    let game gact      = logged_in (Game gact)
    let playing pact   = game (Playing pact)
    let clock cdact    = playing (Clock cdact)
  end

  module State = struct
    type t = { schedule : Action.t -> unit }
  end

  let apply_playing_action
      (t : Playing.Action.t)
      ~(schedule : Action.t -> _)
      ~conn
      ~(login : Logged_in.Model.t)
      (round : Playing.Model.t)
    =
    match t with
    | Market market ->
      let other_hands =
        Map.mapi round.other_hands ~f:(fun ~key:username ~data:hand ->
          Card.Hand.foldi market ~init:hand
            ~f:(fun suit hand book ->
              let size =
                List.sum (module Size) book.sell ~f:(fun order ->
                  if Username.equal order.owner username
                  then order.size
                  else Size.zero)
              in
              Partial_hand.selling hand ~suit ~size))
      in
      { round with market; other_hands }
    | Hand hand ->
      { round with my_hand = hand }
    | Set_clock end_time ->
      let clock =
        Countdown.of_end_time
          ~schedule:(fun cdact -> schedule (Action.clock cdact))
          end_time
      in
      { round with clock = Some clock }
    | Clock cdact ->
      let clock =
        Option.map round.clock ~f:(fun c ->
          Countdown.Action.apply cdact
            ~schedule:(fun cdact -> schedule (Action.clock cdact))
            c)
      in
      { round with clock }
    | Trade (order, with_) ->
      let trades = Fqueue.enqueue round.trades (order, with_) in
      let other_hands =
        Map.mapi round.other_hands ~f:(fun ~key:username ~data:hand ->
          let traded dir =
            Partial_hand.traded
              hand ~suit:order.symbol ~size:order.size ~dir
          in
          if Username.equal username order.owner
          then traded order.dir
          else if Username.equal username with_
          then traded (Dir.other order.dir)
          else hand)
      in
      { round with trades; other_hands }
    | Scroll_trades act ->
      { round with
        trades_scroll = Scrolling.apply_action round.trades_scroll act
      }
    | Score _ -> round
    | Send_order { symbol; dir; price } ->
      let order =
        { Order.owner = login.me.username
        ; id = round.next_order
        ; symbol; dir; price
        ; size = Size.of_int 1
        }
      in
      don't_wait_for begin
        Rpc.Rpc.dispatch_exn Protocol.Order.rpc conn order
        >>| function
        | Ok `Ack -> ()
        | Error reject ->
          schedule (Add_message (Order_reject (order, reject)))
      end;
      let next_order = Order.Id.next round.next_order in
      { round with next_order }
    | Send_cancel (By_id oid) ->
      don't_wait_for begin
        Rpc.Rpc.dispatch_exn Protocol.Cancel.rpc conn oid
        >>| function
        | Ok `Ack -> ()
        | Error reject ->
          schedule (Add_message (Cancel_reject (`Id oid, reject)))
      end;
      round
    | Send_cancel (By_symbol_side { symbol; dir }) ->
      don't_wait_for begin
        Card.Hand.get round.market ~suit:symbol
        |> Dirpair.get ~dir
        |> Deferred.List.iter ~how:`Parallel ~f:(fun order ->
            if Cpty.equal order.owner login.me.username
            then begin
              Rpc.Rpc.dispatch_exn Protocol.Cancel.rpc conn order.id
              >>| function
              | Ok `Ack -> ()
              | Error reject ->
                schedule (Add_message (Cancel_reject (`Id order.id, reject)))
            end
            else Deferred.unit)
      end;
      round
    | Send_cancel All ->
      don't_wait_for begin
        Rpc.Rpc.dispatch_exn Protocol.Cancel_all.rpc conn ()
        >>| function
        | Ok `Ack -> ()
        | Error reject ->
          let reject = (reject :> Protocol.Cancel.error) in
          schedule (Add_message (Cancel_reject (`All, reject)))
      end;
      round

  let apply_game_action
      (t : Game.Action.t)
      ~schedule ~conn ~(login : Logged_in.Model.t)
      (game : Game.Model.t) : Game.Model.t
    =
    match game, t with
    | Waiting wait, I'm_ready readiness ->
      don't_wait_for begin
        Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc conn readiness
        >>| function
        | Ok () | Error `Already_playing | Error `Login_first -> ()
      end;
      Waiting
        (Waiting.set_player_readiness wait
          ~username:login.me.username
          ~is_ready:readiness)
    | Playing round, Playing pact ->
      Playing (apply_playing_action pact ~schedule ~conn ~login round)
    | Waiting _wait, Start_playing ->
      schedule (Add_message New_round);
      let trades_scroll = Scrolling.Model.create ~id:Ids.tape in
      Playing
        { my_hand = Card.Hand.create_all Size.zero
        ; other_hands =
            Map.map login.others ~f:(fun _ ->
              Partial_hand.create_unknown (Size.of_int 10))
        ; market = Book.empty
        ; trades = Fqueue.empty
        ; trades_scroll
        ; next_order = Order.Id.zero
        ; clock = None
        }
    | Playing _round, Round_over ->
      Waiting { ready = Username.Set.empty }
    | _ -> game

  let apply_logged_in_action
      (t : Logged_in.Action.t)
      ~schedule ~conn
      (login : Logged_in.Model.t) : Logged_in.Model.t
    =
    match t with
    | Game gact ->
      let game = apply_game_action gact ~schedule ~conn ~login login.game in
      { login with game }
    | Update up ->
      let just_schedule act = schedule act; login in
      match up with
      | Hand hand     -> just_schedule (Action.playing (Hand hand))
      | Market market -> just_schedule (Action.playing (Market market))
      | Broadcast (Exec (order, exec)) ->
        List.iter exec.fully_filled ~f:(fun filled_order ->
          schedule (Action.playing (Trade
            ( { order
                with size = filled_order.size; price = filled_order.price }
            , filled_order.owner
            ))));
        Option.iter exec.partially_filled ~f:(fun partial_fill ->
          schedule (Action.playing (Trade
            ( { order with size = partial_fill.filled_by }
            , partial_fill.original_order.owner
            ))));
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
        login
      | Broadcast (Player_joined username) ->
        Logged_in.add_new_player login ~username
      | Broadcast New_round ->
        schedule (Action.game Start_playing);
        don't_wait_for begin
          (* Sample the time *before* we send the RPC. Then the game end
             time we get is actually roughly "last time we can expect to
             send an RPC and have it arrive before the game ends". *)
          let current_time = Time_ns.now () in
          Rpc.Rpc.dispatch_exn Protocol.Time_remaining.rpc conn ()
          >>| function
          | Error `Game_not_in_progress -> ()
          | Ok remaining ->
            let end_time = Time_ns.add current_time remaining in
            schedule (Action.playing (Set_clock end_time))
        end;
        login
      | Broadcast (Round_over results) ->
        schedule (Action.game Round_over);
        schedule (Add_message (Round_over results));
        login
      | Broadcast (Scores scores) ->
        Map.fold scores ~init:login
          ~f:(fun ~key:username ~data:score login ->
            Logged_in.update_player login ~username
              ~f:(fun player -> { player with score }))
      | Broadcast (Chat (who, msg)) ->
        let player =
          Option.value (Logged_in.player login ~username:who)
            ~default:Player.Persistent.nobody
        in
        let class_ = Player.Persistent.class_ player in
        just_schedule (Add_message (Chat ((who, class_), msg)))
      | Broadcast (Out _) ->
        don't_wait_for begin
          Rpc.Rpc.dispatch_exn Protocol.Get_update.rpc conn Market
          >>| Protocol.playing_exn
        end;
        login
      | Broadcast (Player_ready { who; is_ready }) ->
        Logged_in.update_ready_if_waiting login ~f:(fun wait ->
          Waiting.set_player_readiness wait ~username:who ~is_ready)

  let apply_connected_action
      (t : Connected.Action.t)
      ~schedule
      (conn : Connected.Model.t) : Connected.Model.t
    =
    match t with
    | Start_login username ->
      don't_wait_for begin
        Rpc.Pipe_rpc.dispatch_exn Protocol.Login.rpc conn.conn username
        >>= fun (pipe, _pipe_metadata) ->
        schedule (Action.connected (Finish_login username));
        Pipe.iter_without_pushback pipe
          ~f:(fun update -> schedule (Action.logged_in (Update update)))
        >>| fun () ->
        schedule Connection_failed
      end;
      conn
    | Finish_login username ->
      let me : Player.Persistent.t =
        { username; style = Me; score = Price.zero }
      in
      let logged_in =
        { Logged_in.Model.me; others = Username.Map.empty
        ; game = Waiting { ready = Username.Set.empty }
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
    | Scroll_chat act ->
      { model with messages = Chat.apply_scrolling_action model.messages act }
    | Start_connecting host_and_port ->
      let host, port = Host_and_port.tuple host_and_port in
      don't_wait_for begin
        Websocket_rpc_transport.connect (Host_and_port.create ~host ~port)
        >>= function
        | Error () ->
          state.schedule Connection_failed;
          Deferred.unit
        | Ok transport ->
          Rpc.Connection.create
            ~connection_state:ignore
            transport
          >>| function
          | Error exn ->
            ignore exn;
            state.schedule Connection_failed
          | Ok conn ->
            state.schedule (Finish_connecting { host_and_port; conn })
      end;
      { model with state = Connecting host_and_port }
    | Finish_connecting { host_and_port; conn } ->
      { model with state = Connected { host_and_port; conn; login = None } }
    | Connection_failed ->
      { model with state = Connection_failed }
    | Connected cact ->
      begin match model.state with
      | Connected conn ->
        let conn =
          apply_connected_action cact ~schedule:state.schedule conn
        in
        { model with state = Connected conn }
      | _ -> model
      end

  let parse_host_and_port hps =
    match Host_and_port.of_string hps with
    | exception _ ->
      Host_and_port.create
        ~host:hps
        ~port:Protocol.default_websocket_port
    | host_and_port -> host_and_port

  let status_line
      ~(inject : Action.t -> _)
      (state : Model.Connection_state.t)
    =
    let class_, status =
      let textf fmt = ksprintf (fun s -> Node.text s) fmt in
      match state with
      | Connecting host_and_port ->
        ( "Connecting"
        , [textf !"Connecting to %{Host_and_port}" host_and_port]
        )
      | Connected { login = None; host_and_port; conn = _ } ->
        ( "Connected"
        , [textf !"Connected to %{Host_and_port}" host_and_port]
        )
      | Connected { login = Some login; _ } ->
        ( "LoggedIn"
        , begin match login.game with
          | Waiting _ -> [textf "Waiting for players"]
          | Playing _ -> [textf "Play!"]
          end
        )
      | Connection_failed ->
        ( "ConnectionFailed"
        , [textf "Connection failed"]
        )
      | Disconnected ->
        let address_from_query_string =
          List.Assoc.find Url.Current.arguments
            ~equal:String.Caseless.equal "address"
        in
        ( "Disconnected"
        , [ textf "Connect to:"
          ; Widget.textbox ~id:Ids.connectTo
              ?initial_value:address_from_query_string
              ~placeholder:"host[:port]" ~clear_on_submit:false
              ~on_submit:(fun hps ->
                inject (Start_connecting (parse_host_and_port hps)))
              ()
          ]
        )
    in
    let clock =
      match state with
      | Connected { login = Some { game = Playing { clock; _}; _ }; _ } ->
        Option.map clock ~f:(fun clock ->
          (Node.span [Attr.class_ "clock"]
            [Node.text (Countdown.Model.to_string clock)]))
      | _ -> None
    in
    Node.p [Attr.id "status"; Attr.class_ class_]
      (status @ Option.to_list clock)

  let order_entry ~(market : Book.t) ~inject ~hotkeys ~symbol ~dir =
    let id = Ids.order ~dir ~suit:symbol in
    let hotkey = Hotkeys.lookup_id hotkeys id in
    let placeholder = Hotkeys.placeholder_of_id hotkeys id in
    let book = Card.Hand.get market ~suit:symbol in
    Widget.textbox ~id ?placeholder
      ~on_keypress:(fun ~self ev ->
        match Hotkeys.char_code ev with
        | None -> Event.Ignore
        | Some ('j' | 'p' as join_penny) ->
            Option.iter (List.hd (Dirpair.get book ~dir)) ~f:(fun order ->
              let price =
                if Char.equal join_penny 'p'
                then
                  Price.make_more_agg order.price
                    ~by:(Market.Price.of_int 1) ~dir
                else
                  order.price
              in
              self##.value := Js.string (Price.to_string price));
            Event.Prevent_default
        | Some other ->
            if Option.exists hotkey ~f:(Char.equal other)
            then begin
              Option.iter (List.hd (Dirpair.get book ~dir:(Dir.other dir)))
                ~f:(fun order ->
                  self##.value := Js.string (Price.to_string order.price));
              Event.Prevent_default
            end else Event.Ignore)
      ~on_submit:(fun price_s ->
        if String.Caseless.equal price_s "x"
        then begin
          inject (Action.playing
            (Send_cancel (By_symbol_side { symbol; dir })))
        end else match Price.of_string price_s with
        | exception _ -> Event.Ignore
        | price ->
          inject (Action.playing (Send_order { symbol; dir; price })))
      ()

  let market_table
    ~hotkeys ~players ~(inject : Action.t -> _) (market : Book.t)
    =
    let market_depth = 3 in
    let nbsp = "\xc2\xa0" in
    let empty_cells =
      List.init market_depth ~f:(fun _ ->
        Node.td [] [Node.text nbsp])
    in
    let input_row ~dir =
      let dir_s = Dir.to_string dir in
      Node.tr [Attr.id ("order" ^ dir_s); Attr.class_ dir_s]
        (List.map Card.Suit.all ~f:(fun symbol ->
          Node.td [] [order_entry ~hotkeys ~market ~inject ~symbol ~dir]))
    in
    let cells ~dir =
      List.map Card.Suit.all ~f:(fun symbol ->
        let halfbook = Dirpair.get (Per_symbol.get market ~symbol) ~dir in
        let cells =
          List.map (List.take halfbook market_depth)
            ~f:(fun order ->
              let player =
                Map.find players order.owner
                |> Option.value ~default:Player.nobody
              in
              Node.td [] [
                Node.span
                  [Attr.class_ (Player.Persistent.class_ player.pers)]
                  [Node.text (Price.to_string order.price)]
              ])
        in
        List.take (cells @ empty_cells) market_depth)
      |> List.transpose_exn
      |> Dir.fold dir ~buy:Fn.id ~sell:List.rev
      |> List.map ~f:(fun row ->
          Node.tr [Attr.class_ (Dir.to_string dir)] row)
    in
    Node.table [Attr.id "market"]
      (List.concat
        [ cells ~dir:Sell
        ; [input_row ~dir:Sell]
        ; [Node.tr [Attr.id "suits"]
            (List.map Card.Suit.all ~f:(fun suit ->
              Node.td [Attr.class_ (Card.Suit.name suit)]
                [Node.text (Card.Suit.to_utf8 suit)]))]
        ; [input_row ~dir:Buy]
        ; cells ~dir:Buy
        ])

  let tape_table
      ~(my_username : Username.t)
      ~(players : Player.t Username.Map.t)
      (market : Book.t)
      trades
      =
    let row_of_order ~include_oid ~traded_with (trade : Order.t) =
      let person_td username =
        let attrs =
          match Map.find players username with
          | None -> []
          | Some other -> Player.Persistent.attrs other.pers
        in
        Node.td []
          [Node.span attrs [Node.text (Username.to_string username)]]
      in
      [ Node.td [Attr.class_ "oid"]
          (if include_oid
          then [Node.text (Order.Id.to_string trade.id)]
          else [])
      ; person_td trade.owner
      ; Node.td [Attr.class_ (Dir.to_string trade.dir)]
          [Node.text (Dir.fold trade.dir ~buy:"B" ~sell:"S")]
      ; begin let size_n =
          if Size.equal trade.size (Size.of_int 1)
          then []
          else [Node.text (Size.to_string trade.size)]
        in
        Node.td []
          (size_n
          @ [ Node.span [Attr.class_ (Card.Suit.name trade.symbol)]
              [Node.text (Card.Suit.to_utf8 trade.symbol)]
            ])
        end
      ; Node.td [Attr.class_ "price"]
          [Node.text (Price.to_string trade.price)]
      ; match traded_with with
        | None -> Node.td [] []
        | Some username -> person_td username
      ]
      |> Node.tr []
    in
    let trades =
      Fqueue.to_list trades
      |> List.map ~f:(fun ((traded : Order.t), with_) ->
          row_of_order ~include_oid:false ~traded_with:(Some with_) traded)
    in
    let open_orders =
      Card.Hand.fold market ~init:[] ~f:(fun acc per_sym ->
        per_sym.buy @ per_sym.sell @ acc)
      |> List.map ~f:(fun order ->
        row_of_order
          ~include_oid:(Username.equal my_username order.owner)
          ~traded_with:None
          order)
    in
    Node.table [Attr.id Ids.tape] (trades @ open_orders)

  let hotkeys =
    [| 'q', Ids.order ~dir:Sell ~suit:Spades
    ;  'w', Ids.order ~dir:Sell ~suit:Hearts
    ;  'e', Ids.order ~dir:Sell ~suit:Diamonds
    ;  'r', Ids.order ~dir:Sell ~suit:Clubs
    ;  'a', Ids.order ~dir:Buy  ~suit:Spades
    ;  's', Ids.order ~dir:Buy  ~suit:Hearts
    ;  'd', Ids.order ~dir:Buy  ~suit:Diamonds
    ;  'f', Ids.order ~dir:Buy  ~suit:Clubs
    ;  'c', Ids.cancel
    |]

  let send_chat (model : Model.t) msg =
    match model.state with
    | Connected { conn; _ } ->
      don't_wait_for begin
        Rpc.Rpc.dispatch_exn Protocol.Chat.rpc conn msg
        >>| function
        | Error `Login_first | Ok () -> ()
      end
    | _ -> ()

  let chat_view (model : Model.t) ~(inject : Action.t -> _) =
    let chat_inject : Chat.Action.t -> _ = function
      | Send_chat msg ->
        send_chat model msg;
        Event.Ignore
      | Scroll_chat act ->
        inject (Scroll_chat act)
    in
    let is_connected =
      match model.state with
      | Connected _ -> true
      | _ -> false
    in
    Chat.view model.messages ~is_connected ~inject:chat_inject

  let view (incr_model : Model.t Incr.t) ~inject =
    let open Incr.Let_syntax in
    let%map model = incr_model in
    let infoboxes =
      match model.state with
      | Connected { login = None; _ } ->
        Infobox.login ~inject_login:(fun user ->
          inject (Action.connected (Start_login user)))
      | Connected { login = Some login; _ } ->
        begin match login.game with
        | Playing playing ->
          let others = 
            Map.map login.others ~f:(fun pers ->
              let hand =
                Option.value
                  ~default:Partial_hand.empty
                  (Map.find playing.other_hands pers.username)
              in
              { Player.pers; hand })
          in
          let my_partial_hand =
            Partial_hand.create_known playing.my_hand
          in
          let me = { Player.pers = login.me; hand = my_partial_hand } in
          Infobox.playing ~others ~me
        | Waiting { ready } ->
          Infobox.waiting
            ~inject_I'm_ready:(fun readiness ->
              inject (Action.game (I'm_ready readiness)))
            ~others:login.others
            ~me:login.me
            ~who_is_ready:ready
        end
      | _ -> Infobox.empty
    in
    let me, others, exchange =
      match model.state with
      | Connected { login = Some login; _ } ->
        begin match login.game with
        | Playing { my_hand; other_hands; market; trades; _ } ->
          let me =
            { Player.pers = login.me
            ; hand = Partial_hand.create_known my_hand
            }
          in
          let others =
            Map.merge login.others other_hands ~f:(fun ~key:_ ->
              function
              | `Both (pers, hand) -> Some { Player.pers; hand }
              | _ -> None)
          in
          (me, others, Some (market, trades))
        | Waiting _ ->
          ( Player.with_empty_hand login.me
          , Map.map login.others ~f:Player.with_empty_hand
          , None
          )
        end
      | _ -> (Player.nobody, Username.Map.empty, None)
    in
    let players = Map.add others ~key:me.pers.username ~data:me in
    let market, trades =
      Option.value exchange ~default:(Book.empty, Fqueue.empty)
    in
    let my_username = me.pers.username in
    let on_keypress = Vdom.Attr.on_keypress (Hotkeys.on_keypress hotkeys) in
    let open Node in
    body [on_keypress] [div [Attr.id "container"]
      [ status_line ~inject model.state
      ; table [Attr.id "exchange"]
        [ tr []
          [ td [] [market_table ~hotkeys ~players ~inject market]
          ; td [] [tape_table ~my_username ~players market trades]
          ]
        ]
      ; infoboxes
      ; chat_view model ~inject
      ]
    ]

  let on_startup ~schedule _model =
    Option.iter (List.Assoc.find Url.Current.arguments "autoconnect")
      ~f:(fun v -> schedule (Action.Start_connecting (parse_host_and_port v)));
    return { State.schedule }

  let on_display ~(old : Model.t) (new_ : Model.t) (state : State.t) =
    begin match old.state, new_.state with
    | Connecting _, Connected _ ->
      Focus.with_input ~id:Ids.login ~f:(fun input ->
        input##focus;
        let n = String.length (Js.to_string input##.value) in
        input##.selectionStart := n;
        input##.selectionEnd := n)
    | Connected { login = None; _ }, Connected { login = Some _; _ } ->
      (* would like to focus ready button here, but buttonElement doesn't
         seem to have a focus method *)
      ()
    | _, Connected { login = Some { game = Playing p; _ }; _ } ->
      Scrolling.on_display p.trades_scroll
        ~schedule:(fun act ->
          state.schedule (Action.playing (Scroll_trades act)))
    | _ -> ()
    end;
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
