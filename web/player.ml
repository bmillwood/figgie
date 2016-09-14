open Core_kernel.Std
open Async_kernel.Std
open Async_rpc_kernel.Std
open Incr_dom.Std

module Waiting = struct
  module Model = struct
    type t = { not_ready : Username.Set.t }
  end

  module Action = struct
    type t =
      | Player_is_ready of { other : Username.t option; is_ready : bool }
      [@@deriving sexp_of]
  end
end

module Playing = struct
  module Model = struct
    type t = {
      market     : Market.Book.t;
      trades     : (Market.Order.t * Market.Cpty.t) Fqueue.t;
      my_hand    : Market.Size.t Card.Hand.t;
      hands      : Partial_hand.t Username.Map.t;
      next_order : Market.Order.Id.t;
    }
  end

  module Action = struct
    type t =
      | Market of Market.Book.t
      | Hand of Market.Size.t Card.Hand.t
      | Trade of Market.Order.t * Market.Cpty.t
      | Score of Market.Price.t
      | Send_order of {
          symbol : Card.Suit.t;
          dir    : Market.Dir.t; 
          price  : Market.Price.t;
        }
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
      | Waiting of Waiting.Action.t
      | Playing of Playing.Action.t
      | Start_playing
      | Round_over
      [@@deriving sexp_of]
  end 
end

module Player_id = struct
  type t = int

  let class_ t = "them" ^ Int.to_string t
end

module Other_player = struct
  type t = {
    id : Player_id.t;
    score : Market.Price.t;
  }
end

module Logged_in = struct
  module Model = struct
    type t = {
      username   : Username.t;
      others     : Other_player.t Username.Map.t;
      score      : Market.Price.t;
      game       : Game.Model.t;
    }
  end

  module Action = struct
    type t =
      | Player_joined of Username.t
      | Game of Game.Action.t
      [@@deriving sexp_of]
  end
end

module Connected = struct
  module Model = struct
    type t = {
      conn : Rpc.Connection.t;
      login : Logged_in.Model.t option;
    }
  end

  module Action = struct
    type t =
      | Login of Username.t
      | Logged_in of Logged_in.Action.t
      [@@deriving sexp_of]
  end
end

module App = struct
  module Model = struct
    module Connection_state = struct
      type t =
        | Connecting
        | Connected of Connected.Model.t
        | Disconnected
    end

    module Message = struct
      type t =
        | Order_reject of Protocol.Order.error
        [@@deriving sexp_of]
    end

    type t = {
      messages : Message.t Fqueue.t;
      state : Connection_state.t
    }

    let initial =
      { messages = Fqueue.empty
      ; state = Disconnected
      }
  end
  open Model

  module Action = struct
    type t =
      | Message of Message.t
      | Disconnect
      | Start_connecting
      | Finish_connecting of Rpc.Connection.t
      | Connected of Connected.Action.t
      [@@deriving sexp_of]

    let connected cact = Connected cact
    let logged_in lact = connected (Logged_in lact)
    let game gact      = logged_in (Game gact)
    let waiting wact   = game (Waiting wact)
    let playing pact   = game (Playing pact)

    let apply_waiting_action
        (t : Waiting.Action.t)
        ~schedule:_ ~conn ~(login : Logged_in.Model.t)
        ({ not_ready } : Waiting.Model.t)
      =
      match t with
      | Player_is_ready { other; is_ready } ->
        let toggle = if is_ready then Set.remove else Set.add in
        let not_ready =
          toggle not_ready (Option.value other ~default:login.username)
        in
        begin if Option.is_none other 
        then 
          don't_wait_for begin
            Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc conn is_ready
            >>| function
            | Ok () -> ()
            | Error `Already_playing -> ()
            | Error `Login_first -> ()
          end
        end;
        { Waiting.Model.not_ready }

    let apply_playing_action
        (t : Playing.Action.t)
        ~schedule ~conn
        (round : Playing.Model.t)
      =
      match t with
      | Market market -> { round with market }
      | Hand my_hand  -> { round with my_hand }
      | Trade (order, with_) ->
        let trades = Fqueue.enqueue round.trades (order, with_) in
        { round with trades }
      | Score _ -> round
      | Send_order { symbol; dir; price } ->
        let order =
          { Market.Order.owner = Username.of_string "bmillwood"
          ; id = round.next_order
          ; symbol; dir; price
          ; size = Market.Size.of_int 1
          } 
        in
        don't_wait_for begin
          Rpc.Rpc.dispatch_exn Protocol.Order.rpc conn order
          >>| function
          | Ok `Ack -> ()
          | Error reject -> schedule (Message (Order_reject reject))
        end;
        let next_order = Market.Order.Id.next round.next_order in
        { round with next_order }

    let apply_game_action
        (t : Game.Action.t)
        ~schedule ~conn ~login
        (game : Game.Model.t) : Game.Model.t
      =
      match game, t with
      | Waiting wait, Waiting wact ->
        Waiting (apply_waiting_action wact ~schedule ~conn ~login wait)
      | Playing round, Playing pact ->
        Playing (apply_playing_action pact ~schedule ~conn round)
      | Waiting _wait, Start_playing ->
        Playing
          { market  = Market.Book.empty
          ; trades  = Fqueue.empty
          ; my_hand = Card.Hand.create_all Market.Size.zero
          ; hands =
              Map.map login.others ~f:(fun _ ->
                Partial_hand.create_unknown (Market.Size.of_int 10))
          ; next_order = Market.Order.Id.zero
          }
      | Playing _round, Round_over ->
        Waiting
          { not_ready = Username.Set.of_list (Map.keys login.others) }
      | _ -> game

    let apply_logged_in_action
        (t : Logged_in.Action.t)
        ~schedule ~conn
        (login : Logged_in.Model.t) : Logged_in.Model.t
      =
      match t with
      | Player_joined their_name ->
        let id =
          let rec loop n =
            if Map.exists login.others ~f:(fun other -> n = other.id)
            then loop (n + 1)
            else n
          in
          loop 1
        in
        let others =
          Map.add login.others
            ~key:their_name
            ~data:{ id; score = Market.Price.zero }
        in
        schedule
          (waiting (Player_is_ready
            { other = Some their_name; is_ready = false }));
        { login with others }        
      | Game gact ->
        let game = apply_game_action gact ~schedule ~conn ~login login.game in
        { login with game }

    let apply_connected_action
        (t : Connected.Action.t)
        ~schedule
        (conn : Connected.Model.t) : Connected.Model.t
      =
      match t with
      | Login username ->
        let logged_in =
          { Logged_in.Model.username; others = Username.Map.empty
          ; score = Market.Price.of_int 0
          ; game = Waiting { not_ready = Username.Set.singleton username }
          }
        in
        { conn = conn.conn; login = Some logged_in }
      | Logged_in lact ->
        begin match conn.login with
        | None -> conn
        | Some logged_in ->
          let logged_in =
            apply_logged_in_action lact ~schedule ~conn:conn.conn logged_in
          in
          { conn with login = Some logged_in }
        end

    let apply t ~schedule (model : Model.t) =
      match t with
      | Message msg ->
        { model with messages = Fqueue.enqueue model.messages msg }
      | Disconnect ->
        { model with state = Disconnected }
      | Start_connecting ->
        { model with state = Connecting }
      | Finish_connecting conn ->
        { model with state = Connected { conn; login = None } }
      | Connected cact ->
        begin match model.state with
        | Connected conn ->
          let conn = apply_connected_action cact ~schedule conn in
          { model with state = Connected conn }
        | _ -> model
        end

    let should_log _ = false
  end

  let status_line
      ~(inject : Action.t -> _)
      (state : Model.Connection_state.t)
    =
    let ready_button is_ready =
      Vdom.Node.button
        [ Vdom.Attr.on_click (fun _ev ->
            inject (Action.waiting
              (Player_is_ready { other = None; is_ready = not is_ready })))
        ; Vdom.Attr.id "readybutton"
        ]
        [ Vdom.Node.text (if is_ready then "Not ready" else "Ready") ]
    in
    let status_text, is_ready =
      match state with
      | Connecting -> "Connecting", None
      | Connected { login = None; _ } -> "Connected", None
      | Connected { login = Some login; _ } ->
        begin match login.game with
        | Waiting { not_ready } ->
          ( "Waiting for players"
          , Some (not (Set.mem not_ready login.username))
          )
        | Playing _ -> "Play!", None
        end
      | Disconnected -> "Disconnected", None
    in
    List.filter_opt
      [ Some (Vdom.Node.text status_text)
      ; Option.map is_ready ~f:ready_button
      ; Some (Vdom.Node.span [Vdom.Attr.class_ "clock"]
          [Vdom.Node.text "MM:SS"])
      ]
    |> Vdom.Node.p [Vdom.Attr.id "status"; Vdom.Attr.class_ status_text]

  let market_table ~(inject : Action.t -> _) (market : Market.Book.t) =
    let open Market in
    let market_depth = 3 in
    let nbsp = "\xc2\xa0" in
    let empty_cells =
      List.init market_depth ~f:(fun _ ->
        Vdom.Node.td [] [Vdom.Node.text nbsp])
    in
    let input_row ~dir =
      let id symbol = "order" ^ Dir.to_string dir ^ Card.Suit.name symbol in
      let keys =
        Dir.fold dir
          ~sell:["Q"; "W"; "E"; "R"]
          ~buy:["A"; "S"; "D"; "F"]
      in
      let handle_keypress ~symbol ~my_hotkey:_ keyboardEvent =
        let open Result.Monad_infix in
        match
          Result.of_option
            (Option.some_if (keyboardEvent##.keyCode = 13) ())
            ~error:"not 13"
          >>= fun () ->
          Result.of_option
            (Js.Opt.to_option keyboardEvent##.target)
            ~error:"no target"
          >>= fun target ->
          Result.of_option
            (Js.Opt.to_option (Dom_html.CoerceTo.input target))
            ~error:"target is not input"
          >>= fun input ->
          Result.of_option
            (Option.try_with (fun () ->
              Market.Price.of_string (Js.to_string input##.value)))
            ~error:"not price"
          >>| fun price ->
          input##.value := Js.string "";
          inject (Action.playing (Send_order { symbol; dir; price }))
        with
        | Ok ev -> ev
        | Error e -> ignore e; Vdom.Event.Ignore
      in
      Vdom.Node.tr [Vdom.Attr.id ("order" ^ Dir.to_string dir)]
        (List.map2_exn Card.Suit.all keys ~f:(fun symbol key ->
          Vdom.Node.td [] [
            Vdom.Node.input
              [ Vdom.Attr.type_ "text"
              ; Vdom.Attr.id (id symbol)
              ; Vdom.Attr.placeholder key
              ; Vdom.Attr.on_keypress
                (handle_keypress ~symbol ~my_hotkey:key)
              ]
              []
          ]
        ))
    in
    let cells ~dir =
      List.map Card.Suit.all ~f:(fun symbol ->
        let halfbook = Dirpair.get (Per_symbol.get market ~symbol) ~dir in
        let cells =
          List.map (List.take halfbook market_depth)
            ~f:(fun order ->
              Vdom.Node.td [] [
                Vdom.Node.span
                  [Vdom.Attr.class_ (Cpty.to_string order.owner)]
                  [Vdom.Node.text (Price.to_string order.price)]
              ])
        in
        List.take (cells @ empty_cells) market_depth)
      |> List.transpose_exn
      |> Dir.fold dir ~buy:Fn.id ~sell:List.rev
      |> List.map ~f:(fun row ->
          Vdom.Node.tr [Vdom.Attr.class_ (Dir.to_string dir)] row)
    in
    Vdom.Node.table [Vdom.Attr.id "market"]
      (List.concat
        [ cells ~dir:Sell
        ; [input_row ~dir:Sell]
        ; [Vdom.Node.tr [Vdom.Attr.id "suits"]
            (List.map Card.Suit.all ~f:(fun suit ->
              Vdom.Node.td [Vdom.Attr.class_ (Card.Suit.name suit)]
                [Vdom.Node.text (Card.Suit.to_utf8 suit)]))]
        ; [input_row ~dir:Buy]
        ; cells ~dir:Buy
        ])

  let trades_table ~(others : Other_player.t Username.Map.t) trades =
    Vdom.Node.table [Vdom.Attr.id "tape"]
      (Fqueue.to_list trades
      |> List.map ~f:(fun ((traded : Market.Order.t), with_) ->
          let size_s = Market.Size.to_string traded.size in
          let size_s = if String.equal size_s "1" then "" else size_s in
          let open Vdom in
          let open Node in
          let cpty_td cpty =
            let attrs =
              match Map.find others cpty with
              | None -> []
              | Some other -> [Attr.class_ (Player_id.class_ other.id)]
            in
            td [] [span attrs [text (Market.Cpty.to_string cpty)]]
          in
          tr []
            [ cpty_td traded.owner
            ; td [Attr.class_ (Market.Dir.to_string traded.dir)]
                [text (Market.Dir.fold traded.dir ~buy:"B" ~sell:"S")]
            ; td []
                [ text size_s
                ; span [Attr.class_ (Card.Suit.name traded.symbol)]
                    [text (Card.Suit.to_utf8 traded.symbol)]
                ]
            ; td [Attr.class_ "price"]
                [text (Market.Price.to_string traded.price)]
            ; cpty_td with_
            ]))

  let history messages =
    Vdom.Node.ul [Vdom.Attr.id "history"]
      (List.map (Fqueue.to_list messages) ~f:(fun msg ->
        Vdom.Node.li []
          [Vdom.Node.text (Sexp.to_string [%sexp (msg : Message.t)])]))

  let view (incr_model : Model.t Incr.t) ~inject =
    let open Incr.Let_syntax in
    let%map model = incr_model in
    let market, others, trades =
      match model.state with
      | Connected
          { login = Some
            { others
            ; game = Playing { market; trades; _ }
            ; _ }
          } ->
        market, others, trades
      | _ -> Market.Book.empty, Username.Map.empty, Fqueue.empty
    in
    let open Vdom in
    let open Node in
    body [] [div [Attr.id "container"]
      [ status_line ~inject model.state
      ; div [Attr.id "exchange"]
        [ market_table ~inject market
        ; trades_table ~others trades
        ; div [Attr.class_ "hands"] []
        ; div [Attr.id "historycmd"]
          [ history model.messages
          ; input [Attr.type_ "text"; Attr.id "cmdline"] []
          ]
        ]
      ]
    ]

  let on_startup ~schedule _model =
    don't_wait_for begin
      let handle_update conn : Protocol.Player_update.t -> unit =
        function
        | Hand hand     -> schedule (Action.playing (Hand hand))
        | Market market -> schedule (Action.playing (Market market))
        | Broadcast (Exec (order, exec)) ->
          List.iter exec.fully_filled ~f:(fun filled_order ->
            schedule (Action.playing (Trade
              ( { order with size = filled_order.size }
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
          end
        | Broadcast (Player_joined username) ->
          schedule (Action.logged_in (Player_joined username))
        | Broadcast New_round ->
          schedule (Action.game Start_playing)
        | Broadcast _ -> ()
      in
      schedule Start_connecting;
      Websocket_rpc_transport.connect
        (Host_and_port.create ~host:"localhost" ~port:20406)
      >>= function
      | Error () ->
        schedule Disconnect;
        Deferred.unit
      | Ok transport ->
        Rpc.Connection.with_close
          ~connection_state:(fun _ -> ())
          transport
          ~on_handshake_error:`Raise
          ~dispatch_queries:(fun conn ->
            schedule (Finish_connecting conn);
            Rpc.Pipe_rpc.dispatch_exn
              Protocol.Login.rpc conn (Username.of_string "bmillwood")
            >>= fun (pipe, _pipe_metadata) ->
            schedule (Action.connected
              (Login (Username.of_string "bmillwood")));
            Pipe.iter_without_pushback pipe
              ~f:(fun update -> handle_update conn update)
            >>| fun () ->
            schedule Disconnect)
    end

  let on_display ~schedule:_ ~old:_ _new = ()
  let update_visibility model = model
end

let () =
  Start_app.simple
    ~initial_state:App.Model.initial
    (module App)
