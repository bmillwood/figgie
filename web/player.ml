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

module Player_id = struct
  type t =
    | Me
    | Them of int
    | Nobody
    [@@deriving compare, sexp]

  let equal t1 t2 = compare t1 t2 = 0

  let class_ =
    function
    | Me -> "me"
    | Them i -> "them" ^ Int.to_string i
    | Nobody -> "nobody"
end

module Player = struct
  module Persistent = struct
    type t = {
      id : Player_id.t;
      username : Username.t;
      score : Market.Price.t;
    } [@@deriving sexp]

    let nobody =
      { id = Nobody
      ; username = Username.of_string "[nobody]"
      ; score = Market.Price.zero
      }
  end

  type t = {
    pers : Persistent.t;
    hand : Partial_hand.t;
  }

  let with_empty_hand pers = { pers; hand = Partial_hand.empty }

  let nobody = with_empty_hand Persistent.nobody
end

module Playing = struct
  module Game_clock = struct
    type t = {
      end_time : Time_ns.t;
      min : int;
      sec : int;
      tick : (unit, unit) Clock_ns.Event.t;
    }

    let initial =
      { end_time = Time_ns.now ()
      ; min = 99; sec = 99
      ; tick = Clock_ns.Event.at (Time_ns.now ())
      }

    let to_string t = sprintf "%02d:%02d" t.min t.sec
  end

  module Model = struct
    type t = {
      me         : Player.t;
      others     : Player.t Username.Map.t;
      market     : Market.Book.t;
      trades     : (Market.Order.t * Market.Cpty.t) Fqueue.t;
      next_order : Market.Order.Id.t;
      clock      : Game_clock.t;
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
      | Game_ends_at of Time_ns.t sexp_opaque
      | Tick of { min : int; sec : int }
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

module Logged_in = struct
  module Model = struct
    type t = {
      me     : Player.Persistent.t;
      others : Player.Persistent.t Username.Map.t;
      game   : Game.Model.t;
    }
  end

  let new_player (t : Model.t) ~username =
    let id =
      let rec loop n =
        if Map.exists t.others
          ~f:(fun other -> Player_id.equal other.id (Them n))
        then loop (n + 1)
        else n
      in
      loop 1
    in
    { Player.Persistent.username; id = Them id; score = Market.Price.zero }

  let update_round_if_playing (t : Model.t) ~f =
    match t.game with
    | Playing round -> { t with game = Playing (f round) }
    | Waiting _ -> t

  (* this function reveals a weakness in my data model :( *)
  let update_player (t : Model.t) ~username ~f =
    if Username.equal t.me.username username
    then begin
      let new_me = f t.me in
      { t with me = new_me }
      |> update_round_if_playing
        ~f:(fun round -> { round with me = { round.me with pers = new_me } })
    end
    else
      let others =
        Map.update t.others username
          ~f:(fun maybe_existing ->
            Option.value maybe_existing ~default:(new_player t ~username)
            |> f)
      in
      { t with others }
      |> update_round_if_playing ~f:(fun round ->
        let new_others =
          Map.map round.others ~f:(fun p ->
            match Map.find others p.pers.username with
            | Some new_pers -> { p with pers = new_pers }
            | None -> p)
        in
        { round with others = new_others })

  let add_new_player (t : Model.t) ~username =
    update_player t ~username ~f:Fn.id

  module Action = struct
    type t =
      | Player_joined of Username.t
      | Scores of Market.Price.t Username.Map.t
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
        | Chat of Username.t * string
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
          toggle not_ready (Option.value other ~default:login.me.username)
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
      | Market market ->
        let others =
          Map.map round.others ~f:(fun player ->
            let hand =
              Card.Hand.foldi market ~init:player.hand
                ~f:(fun suit hand book ->
                  let size =
                    List.sum (module Market.Size) book.sell ~f:(fun order ->
                      if Username.equal order.owner player.pers.username
                      then order.size
                      else Market.Size.zero)
                  in
                  Partial_hand.selling hand ~suit ~size)
            in
            { player with hand })
        in
        { round with market; others }
      | Hand hand ->
        let hand = Partial_hand.create_known hand in
        { round with me = { round.me with hand } }
      | Game_ends_at end_time ->
        Clock_ns.Event.abort_if_possible round.clock.tick ();
        let this_time = Time_ns.now () in
        let remaining = Time_ns.diff end_time this_time in
        let { Time_ns.Span.Parts.sign; min; sec; _ } =
          Time_ns.Span.to_parts remaining
        in
        begin match sign with
        | Pos when min > 0 || sec > 0 ->
          schedule (playing (Tick { min; sec }));
        | _ -> ()
        end;
        { round with clock = { round.clock with end_time } }
      | Tick { min; sec } ->
        let schedule_next_tick ~min ~sec =
          let next_tick =
            Time_ns.sub round.clock.end_time
              (Time_ns.Span.create ~min ~sec ())
          in
          let tick = Clock_ns.Event.at next_tick in
          upon (Clock_ns.Event.fired tick)
            (function
            | `Aborted () -> ()
            | `Happened () -> schedule (playing (Tick { min; sec })));
          { round.clock with tick }
        in
        let clock =
          if sec > 0
          then schedule_next_tick ~min ~sec:(sec - 1)
          else if min > 0
          then schedule_next_tick ~min:(min - 1) ~sec:59
          else round.clock
        in
        { round with clock = { clock with min; sec } }
      | Trade (order, with_) ->
        let trades = Fqueue.enqueue round.trades (order, with_) in
        let others =
          Map.map round.others ~f:(fun player ->
            let traded dir =
              Partial_hand.traded
                player.hand ~suit:order.symbol ~size:order.size ~dir
            in
            if Username.equal player.pers.username order.owner
            then { player with hand = traded order.dir }
            else if Username.equal player.pers.username with_
            then { player with hand = traded (Market.Dir.other order.dir) }
            else player)
        in
        { round with trades; others }
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
          { me = { pers = login.me; hand = Partial_hand.empty }
          ; others =
              Map.map login.others ~f:(fun pers ->
                { Player.pers
                ; hand = Partial_hand.create_unknown (Market.Size.of_int 10)
                })
          ; market  = Market.Book.empty
          ; trades  = Fqueue.empty
          ; next_order = Market.Order.Id.zero
          ; clock = Playing.Game_clock.initial
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
        schedule
          (waiting (Player_is_ready
            { other = Some their_name; is_ready = false }));
        Logged_in.add_new_player login ~username:their_name
      | Scores scores ->
        Map.fold scores ~init:login
          ~f:(fun ~key:username ~data:score login ->
            Logged_in.update_player login ~username
              ~f:(fun player -> { player with score }))
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
        let me : Player.Persistent.t =
          { username; id = Me; score = Market.Price.zero }
        in
        let logged_in =
          { Logged_in.Model.me; others = Username.Map.empty
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
          , Some (not (Set.mem not_ready login.me.username))
          )
        | Playing _ -> "Play!", None
        end
      | Disconnected -> "Disconnected", None
    in
    let clock =
      match state with
      | Connected { login = Some { game = Playing { clock; _}; _ }; _ } ->
        Some (Vdom.Node.span [Vdom.Attr.class_ "clock"]
          [Vdom.Node.text (Playing.Game_clock.to_string clock)])
      | _ -> None
    in
    List.filter_opt
      [ Some (Vdom.Node.text status_text)
      ; Option.map is_ready ~f:ready_button
      ; clock
      ]
    |> Vdom.Node.p [Vdom.Attr.id "status"; Vdom.Attr.class_ status_text]

  let market_table ~players ~(inject : Action.t -> _) (market : Market.Book.t) =
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
        match
          let open Result.Monad_infix in
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
              let player =
                Map.find players order.owner
                |> Option.value ~default:Player.nobody
              in
              let class_ = Player_id.class_ player.pers.id in
              Vdom.Node.td [] [
                Vdom.Node.span
                  [Vdom.Attr.class_ class_]
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

  let trades_table ~(players : Player.t Username.Map.t) trades =
    Vdom.Node.table [Vdom.Attr.id "tape"]
      (Fqueue.to_list trades
      |> List.map ~f:(fun ((traded : Market.Order.t), with_) ->
          let size_s = Market.Size.to_string traded.size in
          let size_s = if String.equal size_s "1" then "" else size_s in
          let open Vdom in
          let open Node in
          let cpty_td cpty =
            let attrs =
              match Map.find players cpty with
              | None -> []
              | Some other -> [Attr.class_ (Player_id.class_ other.pers.id)]
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

  let player_infoboxes ~(me : Player.t) ~(others : Player.t Username.Map.t) =
    let draw_hand ~pos (player : Player.t) =
      let nbsp = "\xc2\xa0" in
      let open Vdom in
      let span_of_copies class_ n s =
        let content =
          List.init (Market.Size.to_int n) ~f:(fun _ -> s)
          |> String.concat
          |> Node.text
        in
        Node.span [Attr.class_ class_] [content]
      in
      let ranking =
        match
          Map.count (Map.add others ~key:me.pers.username ~data:me)
            ~f:(fun o -> Market.Price.O.(o.pers.score > player.pers.score))
        with
        | 0 -> "first"
        | 1 -> "second"
        | 2 -> "third"
        | _ -> "last"
      in
      let unknown =
        span_of_copies "Unknown" player.hand.unknown Partial_hand.unknown_utf8
      in
      let open Vdom in
      List.concat
        [ [ Node.span [Attr.class_ (Player_id.class_ player.pers.id)]
              [ Node.text (Username.to_string player.pers.username) ]
          ; Node.span [Attr.class_ ranking]
              [ Node.text (Market.Price.to_string player.pers.score) ]
          ; Node.create "br" [] []
          ; Node.text nbsp
          ]
        ; Card.Hand.foldi player.hand.known
              ~init:[]
              ~f:(fun suit acc count ->
                span_of_copies
                  (Card.Suit.name suit)
                  count
                  (Card.Suit.to_utf8 suit)
                :: acc)
          |> List.rev
        ; [unknown]
        ]
      |> Node.div [Attr.id (pos ^ "hand")]
    in
    match Map.data others @ List.init 3 ~f:(fun _ -> Player.nobody) with
    | left :: mid :: right :: _ ->
      Vdom.Node.div [Vdom.Attr.id "hands"]
        [ Vdom.Node.div [Vdom.Attr.id "otherhands"]
          [ draw_hand ~pos:"left" left
          ; draw_hand ~pos:"middle" mid
          ; draw_hand ~pos:"right" right
          ]
        ; draw_hand ~pos:"my" me
        ]
    | _ -> assert false

  let history ~players ~messages =
    let nodes_of_message : Message.t -> _ =
      function
      | Chat (who, msg) ->
        let player =
          Map.find players who
          |> Option.value ~default:Player.nobody
        in
        [ Vdom.Node.span
            [Vdom.Attr.class_ (Player_id.class_ player.pers.id)]
            [Vdom.Node.text (Username.to_string player.pers.username)]
        ; Vdom.Node.text ": "
        ; Vdom.Node.text msg
        ]
      | Order_reject reject ->
        [ Vdom.Node.text
            (Protocol.Order.sexp_of_error reject |> Sexp.to_string)
        ]
    in
    Vdom.Node.ul [Vdom.Attr.id "history"]
      (List.map (Fqueue.to_list messages) ~f:(fun msg ->
        Vdom.Node.li [] (nodes_of_message msg)))

  let cmdline (model : Model.t) =
    let this_id = "cmdline" in
    Vdom.Node.input
      [ Vdom.Attr.type_ "text"
      ; Vdom.Attr.id this_id
      ; Vdom.Attr.property "disabled"
          (Js.Unsafe.inject (Js.bool
            (match model.state with | Connected _ -> false | _ -> true)))
      ; Vdom.Attr.on_keypress (fun ev ->
          match
            let open Option.Monad_infix in
            Option.some_if (ev##.keyCode = 13) ()
            >>= fun () ->
            Js.Opt.to_option ev##.target
            >>= fun target ->
            Js.Opt.to_option (Dom_html.CoerceTo.input target)
          with
          | None -> Vdom.Event.Ignore
          | Some input ->
            let msg = input##.value |> Js.to_string in
            begin match model.state with
            | Connected { conn; _ } ->
              don't_wait_for begin
                Rpc.Rpc.dispatch_exn Protocol.Chat.rpc conn msg
                >>| function
                | Error `Login_first | Ok () -> ()
              end;
              input##.value := Js.string ""
            | _ -> ()
            end;
          Vdom.Event.Prevent_default)
      ]
      []

  let view (incr_model : Model.t Incr.t) ~inject =
    let open Incr.Let_syntax in
    let%map model = incr_model in
    let me, others, exchange =
      match model.state with
      | Connected { login = Some login; _ } ->
        begin match login.game with
        | Playing { me; others; market; trades; _ } ->
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
      Option.value exchange ~default:(Market.Book.empty, Fqueue.empty)
    in
    let open Vdom in
    let open Node in
    body [] [div [Attr.id "container"]
      [ status_line ~inject model.state
      ; div [Attr.id "exchange"]
        [ market_table ~players ~inject market
        ; trades_table ~players trades
        ]
      ; player_infoboxes ~me ~others
      ; div [Attr.id "historycmd"]
        [ history ~players ~messages:model.messages
        ; cmdline model
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
          end
        | Broadcast (Player_joined username) ->
          schedule (Action.logged_in (Player_joined username))
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
              schedule (Action.playing (Game_ends_at end_time))
          end
        | Broadcast (Round_over _results) ->
          schedule (Action.game Round_over)
        | Broadcast (Scores scores) ->
          schedule (Action.logged_in (Scores scores))
        | Broadcast (Chat (who, msg)) ->
          schedule (Message (Chat (who, msg)))
        | Broadcast (Out _) -> ()
        | Broadcast (Player_ready _) -> ()
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
