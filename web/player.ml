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

  module Action = struct
    type t =
      | I'm_ready of bool
      | Player_is_ready of { other : Username.t; is_ready : bool }
      [@@deriving sexp_of]
  end

  let set_player_readiness (t : Model.t) ~username ~is_ready =
    let apply = if is_ready then Set.add else Set.remove in
    { Model.ready = apply t.ready username }
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
      score : Price.t;
    } [@@deriving sexp]

    let nobody =
      { id = Nobody
      ; username = Username.of_string "[nobody]"
      ; score = Price.zero
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
  module Model = struct
    type t = {
      my_hand     : Size.t Card.Hand.t;
      other_hands : Partial_hand.t Username.Map.t;
      market      : Book.t;
      trades      : (Order.t * Cpty.t) Fqueue.t;
      next_order  : Order.Id.t;
      clock       : Countdown.Model.t option;
    }
  end

  module Action = struct
    type t =
      | Market of Book.t
      | Hand of Size.t Card.Hand.t
      | Trade of Order.t * Cpty.t
      | Score of Price.t
      | Send_order of {
          symbol : Card.Suit.t;
          dir    : Dir.t;
          price  : Price.t;
        }
      | Send_cancel of Order.Id.t
      | Send_cancel_all
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
    { Player.Persistent.username; id = Them id; score = Price.zero }

  let update_round_if_playing (t : Model.t) ~f =
    match t.game with
    | Playing round -> { t with game = Playing (f round) }
    | Waiting _ -> t

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
      { messages : Chat.Message.t Fqueue.t
      ; state : Connection_state.t
      }

    let initial =
      { messages = Fqueue.empty
      ; state = Disconnected
      }

    let cutoff = phys_equal
  end
  open Model

  module Action = struct
    type t =
      | Message of Chat.Message.t
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
    let waiting wact   = game (Waiting wact)
    let playing pact   = game (Playing pact)
    let clock cdact    = playing (Clock cdact)
  end

  module State = struct
    (* I'm pretty sure this is not what you're supposed to do here.
       But I can't easily see what else it's supposed to be. *)
    type t = { schedule : Action.t -> unit }
  end

  let apply_waiting_action
      (t : Waiting.Action.t)
      ~schedule:_ ~conn ~(login : Logged_in.Model.t)
      (waiting : Waiting.Model.t)
    =
    match t with
    | I'm_ready readiness ->
      don't_wait_for begin
        Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc conn readiness
        >>| function
        | Ok () -> ()
        | Error `Already_playing -> ()
        | Error `Login_first -> ()
      end;
      Waiting.set_player_readiness waiting
        ~username:login.me.username
        ~is_ready:readiness
    | Player_is_ready { other; is_ready } ->
      Waiting.set_player_readiness waiting ~username:other ~is_ready

  let apply_playing_action
      (t : Playing.Action.t)
      ~schedule ~conn ~(login : Logged_in.Model.t)
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
        | Error reject -> schedule (Message (Order_reject reject))
      end;
      let next_order = Order.Id.next round.next_order in
      { round with next_order }
    | Send_cancel oid ->
      don't_wait_for begin
        Rpc.Rpc.dispatch_exn Protocol.Cancel.rpc conn oid
        >>| function
        | Ok `Ack -> ()
        | Error reject -> schedule (Message (Cancel_reject reject))
      end;
      round
    | Send_cancel_all ->
      don't_wait_for begin
        Rpc.Rpc.dispatch_exn Protocol.Cancel_all.rpc conn ()
        >>| function
        | Ok `Ack -> ()
        | Error reject ->
          let reject = (reject :> Protocol.Cancel.error) in
          schedule (Message (Cancel_reject reject))
      end;
      round

  let apply_game_action
      (t : Game.Action.t)
      ~schedule ~conn ~login
      (game : Game.Model.t) : Game.Model.t
    =
    match game, t with
    | Waiting wait, Waiting wact ->
      Waiting (apply_waiting_action wact ~schedule ~conn ~login wait)
    | Playing round, Playing pact ->
      Playing (apply_playing_action pact ~schedule ~conn ~login round)
    | Waiting _wait, Start_playing ->
      Playing
        { my_hand = Card.Hand.create_all Size.zero
        ; other_hands =
            Map.map login.others ~f:(fun _ ->
              Partial_hand.create_unknown (Size.of_int 10))
        ; market  = Book.empty
        ; trades  = Fqueue.empty
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
        schedule
          (Action.waiting (Player_is_ready
            { other = username; is_ready = false }));
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
      | Broadcast (Round_over _results) ->
        just_schedule (Action.game Round_over)
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
        let class_ = Player_id.class_ player.id in
        just_schedule (Message (Chat ((who, class_), msg)))
      | Broadcast (Out _) ->
        don't_wait_for begin
          Rpc.Rpc.dispatch_exn Protocol.Get_update.rpc conn Market
          >>| Protocol.playing_exn
        end;
        login
      | Broadcast (Player_ready { who; is_ready }) ->
        just_schedule
          (Action.waiting (Player_is_ready { other = who; is_ready }))

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
        { username; id = Me; score = Price.zero }
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
    | Message msg ->
      { model with messages = Fqueue.enqueue model.messages msg }
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
          List.Assoc.find Url.Current.arguments ~equal:String.equal "address"
        in
        ( "Disconnected"
        , [ textf "Connect to:"
          ; Widget.textbox ~id:Ids.connectTo
              ?initial_value:address_from_query_string
              ~placeholder:"host[:port]" ~clear_on_submit:false
              ~f:(fun hps ->
                inject (Start_connecting (parse_host_and_port hps)))
              []
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
          let id = Ids.order ~dir ~suit:symbol in
          let placeholder = Hotkeys.placeholder_of_id hotkeys id in
          Node.td [] [
            Widget.textbox ~id ?placeholder
              ~f:(fun price_s ->
                match Price.of_string price_s with
                | exception _ -> Event.Ignore
                | price ->
                  inject (Action.playing (Send_order { symbol; dir; price })))
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
              Node.td [] [
                Node.span
                  [Attr.class_ (Player_id.class_ player.pers.id)]
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
          | Some other -> [Attr.class_ (Player_id.class_ other.pers.id)]
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
    Node.table [Attr.id "tape"] (trades @ open_orders)

  let cxl_by_id ~hotkeys ~inject =
    let placeholder = Hotkeys.placeholder_of_id hotkeys Ids.cancel in
    [ Widget.textbox ~id:Ids.cancel ?placeholder
        ~f:(fun oid ->
          if String.equal oid "all"
          then inject (Action.playing Send_cancel_all)
          else
            match Order.Id.of_string oid with
            | exception _ -> Event.Ignore
            | oid -> inject (Action.playing (Send_cancel oid)))
        []
    ; Node.span [Attr.id "cxlhelp"] [Node.text "cancel by id"]
    ]

  module Infobox = struct
    module Position = struct
      type t = | Me | Left | Middle | Right

      let attr t =
        let class_ =
          match t with
          | Me -> "myself"
          | Left -> "left"
          | Middle -> "middle"
          | Right -> "right"
        in
        Attr.class_ class_
    end
  end

  let infobox ~pos ~name ~score ~info =
    Node.div [Infobox.Position.attr pos]
      ([ [name]
      ; Option.to_list score
      ; [Node.create "br" [] []]
      ; info
      ] |> List.concat)

  let infoboxes = Node.div [Attr.id "infoboxes"]

  let login_infoboxes ~inject =
    let initial_value = List.Assoc.find Url.Current.arguments "username" in
    let login_textbox =
      Widget.textbox ~id:Ids.login ~placeholder:"username" ?initial_value
        ~f:(fun user ->
          inject (Action.connected (Start_login (Username.of_string user))))
        [Attr.classes ["name"; "me"]]
    in
    infoboxes
      [infobox ~pos:Me ~name:login_textbox ~score:None ~info:[]]

  let player_infobox ~pos ~(pers : Player.Persistent.t) ~ranking ~info =
    let score =
      Node.span
        [Attr.classes ["score"; ranking]]
        [Node.text (Price.to_string pers.score)]
    in
    let name =
      Node.span
        [ Attr.classes ["name"; Player_id.class_ pers.id] ]
        [ Node.text (Username.to_string pers.username) ]
    in
    infobox ~pos ~name ~score:(Some score) ~info

  let player_infoboxes ~others ~me =
    let (my_pers : Player.Persistent.t), _ = me in
    let players = Map.add others ~key:my_pers.username ~data:me in
    let nobody = Player.Persistent.nobody, [] in
    match Map.data others @ List.init 3 ~f:(fun _ -> nobody) with
    | left :: middle :: right :: _ ->
      let p ~pos ((pers : Player.Persistent.t), info) =
        let better_players =
          Map.count players ~f:(fun (o, _) ->
            Price.O.(o.score > pers.score))
        in
        let ranking =
          match better_players with
          | 0 -> "first"
          | 1 -> "second"
          | 2 -> "third"
          | _ -> "last"
        in
        player_infobox ~pos ~pers ~ranking ~info
      in
      infoboxes
        [ Node.div [Attr.id "others"]
          [ p ~pos:Left   left
          ; p ~pos:Middle middle
          ; p ~pos:Right  right
          ]
        ; p ~pos:Me me
        ]
    | _ -> assert false

  let waiting_infoboxes ~inject
      ~(me : Player.Persistent.t)
      ~(others : Player.Persistent.t Username.Map.t)
      (waiting : Waiting.Model.t)
    =
    let others =
      Map.map others ~f:(fun o ->
        let ready_text =
          if Set.mem waiting.ready o.username
          then "[ready]"
          else "[not ready]"
        in
        (o, [Node.text ready_text]))
    in
    let ready_button =
      let is_ready = Set.mem waiting.ready me.username in
      let text, set_it_to =
        if is_ready
        then "I'm not ready!", false
        else "I'm ready!", true
      in
      Node.button
        [ Attr.id Ids.ready_button
        ; Attr.on_click (fun _mouseEvent ->
            inject (Action.waiting (I'm_ready set_it_to)))
        ]
        [ Node.text text ]
    in
    player_infoboxes
      ~others
      ~me:(me, [ready_button])

  let playing_infoboxes
      ~(login : Logged_in.Model.t)
      ~(playing : Playing.Model.t)
      =
    let span_of_copies class_ n s =
      let content =
        List.init (Size.to_int n) ~f:(fun _ -> s)
        |> String.concat
        |> Node.text
      in
      Node.span [Attr.class_ class_] [content]
    in
    let draw_hand (player : Player.Persistent.t) (hand : Partial_hand.t) =
      let known =
        Card.Hand.foldi hand.known
          ~init:[]
          ~f:(fun suit acc count ->
            span_of_copies
              (Card.Suit.name suit)
              count
              (Card.Suit.to_utf8 suit)
              :: acc)
          |> List.rev
      in
      let unknown =
        span_of_copies "Unknown" hand.unknown Partial_hand.unknown_utf8
      in
      (player, known @ [unknown])
    in
    let draw_other (player : Player.Persistent.t) =
      let hand =
        Option.value ~default:Partial_hand.empty
          (Map.find playing.other_hands player.username)
      in
      draw_hand player hand
    in
    player_infoboxes
      ~others:(Map.map login.others ~f:draw_other)
      ~me:(draw_hand login.me (Partial_hand.create_known playing.my_hand))

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

  let chat_view (model : Model.t) =
    let chat_model : Chat.Model.t =
      { Chat.Model.messages = model.messages
      ; is_connected =
        match model.state with
        | Connected _ -> true
        | _ -> false
      }
    in
    let chat_inject (Chat.Action.Send_chat msg) =
      begin match model.state with
      | Connected { conn; _ } ->
        don't_wait_for begin
          Rpc.Rpc.dispatch_exn Protocol.Chat.rpc conn msg
          >>| function
          | Error `Login_first | Ok () -> ()
        end
      | _ -> ()
      end;
      Event.Ignore
    in
    Chat.view chat_model ~inject:chat_inject

  let view (incr_model : Model.t Incr.t) ~inject =
    let open Incr.Let_syntax in
    let%map model = incr_model in
    let infoboxes =
      match model.state with
      | Connected { login = None; _ } ->
          login_infoboxes ~inject
      | Connected { login = Some login; _ } ->
        begin match login.game with
        | Playing playing ->
          playing_infoboxes ~login ~playing
        | Waiting waiting ->
          waiting_infoboxes ~inject ~me:login.me ~others:login.others waiting
        end
      | _ -> infoboxes []
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
    let market_help = Node.text "" in
    let on_keypress = Vdom.Attr.on_keypress (Hotkeys.on_keypress hotkeys) in
    let open Node in
    body [on_keypress] [div [Attr.id "container"]
      [ status_line ~inject model.state
      ; table [Attr.id "exchange"]
        [ tr []
          [ td [] [market_table ~hotkeys ~players ~inject market]
          ; td [] [tape_table ~my_username ~players market trades]
          ]
        ; tr []
          [ td [] [market_help]
          ; td [Attr.id "cxlcontainer"] (cxl_by_id ~hotkeys ~inject)
          ]
        ]
      ; infoboxes
      ; chat_view model
      ]
    ]

  let on_startup ~schedule _model =
    Option.iter (List.Assoc.find Url.Current.arguments "autoconnect")
      ~f:(fun v -> schedule (Action.Start_connecting (parse_host_and_port v)));
    return { State.schedule }

  let on_display ~(old : Model.t) (new_ : Model.t) (_state : State.t) =
    match old.state, new_.state with
    | Connecting _, Connected _ ->
      Focus.focus_input ~id:Ids.login
    | Connected { login = None; _ }, Connected { login = Some _; _ } ->
      (* would like to focus ready button here, but buttonElement doesn't
         seem to have a focus method *)
      ()
    | _ -> ()

  let update_visibility model = model
end

let () =
  Start_app.simple
    ~initial_model:App.Model.initial
    (module App)
