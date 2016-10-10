open Core_kernel.Std
open Async_kernel.Std
open Async_rpc_kernel.Std
open Incr_dom.Std
open Vdom

open Market

(* only bother putting an id in here if it is referred to
   in more than one place *)
module Ids = struct
  let login = "login"
  let ready_button = "readyButton"
end

let focus_input ~element_id =
  let (>>>) t f = Option.iter t ~f in
  Option.try_with (fun () -> Dom_html.getElementById element_id)
  >>> fun elt ->
  Js.Opt.to_option (Dom_html.CoerceTo.input elt)
  >>> fun input ->
  input##focus

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

  module Action = struct
    type t =
      | Player_joined of Username.t
      | Scores of Price.t Username.Map.t
      | Game of Game.Action.t
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

    module Message = struct
      type t =
        | Order_reject of Protocol.Order.error
        | Cancel_reject of Protocol.Cancel.error
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
      | Start_connecting of Host_and_port.t
      | Finish_connecting of
        { host_and_port : Host_and_port.t
        ; conn : Rpc.Connection.t
        }
      | Connection_failed
      | Connected of Connected.Action.t
      [@@deriving sexp_of]

    let connected cact = Connected cact
    let logged_in lact = connected (Logged_in lact)
    let game gact      = logged_in (Game gact)
    let waiting wact   = game (Waiting wact)
    let playing pact   = game (Playing pact)
    let clock cdact    = playing (Clock cdact)

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
            ~schedule:(fun cdact -> schedule (clock cdact))
            end_time
        in
        { round with clock = Some clock }
      | Clock cdact ->
        let clock =
          Option.map round.clock ~f:(fun c ->
            Countdown.Action.apply cdact
              ~schedule:(fun cdact -> schedule (clock cdact))
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
      | Player_joined their_name ->
        schedule
          (waiting (Player_is_ready
            { other = their_name; is_ready = false }));
        Logged_in.add_new_player login ~username:their_name
      | Scores scores ->
        Map.fold scores ~init:login
          ~f:(fun ~key:username ~data:score login ->
            Logged_in.update_player login ~username
              ~f:(fun player -> { player with score }))
      | Game gact ->
        let game = apply_game_action gact ~schedule ~conn ~login login.game in
        { login with game }

    let handle_update ~schedule conn : Protocol.Player_update.t -> unit =
      function
      | Hand hand     -> schedule (playing (Hand hand))
      | Market market -> schedule (playing (Market market))
      | Broadcast (Exec (order, exec)) ->
        List.iter exec.fully_filled ~f:(fun filled_order ->
          schedule (playing (Trade
            ( { order
                with size = filled_order.size; price = filled_order.price }
            , filled_order.owner
            ))));
        Option.iter exec.partially_filled ~f:(fun partial_fill ->
          schedule (playing (Trade
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
        schedule (logged_in (Player_joined username))
      | Broadcast New_round ->
        schedule (game Start_playing);
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
            schedule (playing (Set_clock end_time))
        end
      | Broadcast (Round_over _results) ->
        schedule (game Round_over)
      | Broadcast (Scores scores) ->
        schedule (logged_in (Scores scores))
      | Broadcast (Chat (who, msg)) ->
        schedule (Message (Chat (who, msg)))
      | Broadcast (Out _) ->
        don't_wait_for begin
          Rpc.Rpc.dispatch_exn Protocol.Get_update.rpc conn Market
          >>| Protocol.playing_exn
        end
      | Broadcast (Player_ready _) -> ()

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
          schedule (connected (Finish_login username));
          Pipe.iter_without_pushback pipe
            ~f:(fun update -> handle_update ~schedule conn.conn update)
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

    let apply t ~schedule (model : Model.t) =
      match t with
      | Message msg ->
        { model with messages = Fqueue.enqueue model.messages msg }
      | Start_connecting host_and_port ->
        let host, port = Host_and_port.tuple host_and_port in
        don't_wait_for begin
          Websocket_rpc_transport.connect (Host_and_port.create ~host ~port)
          >>= function
          | Error () ->
            schedule Connection_failed;
            Deferred.unit
          | Ok transport ->
            Rpc.Connection.create
              ~connection_state:ignore
              transport
            >>| function
            | Error exn ->
              ignore exn;
              schedule Connection_failed
            | Ok conn ->
              schedule (Finish_connecting { host_and_port; conn })
        end;
        { model with state = Connecting host_and_port }
      | Finish_connecting { host_and_port; conn } ->
        { model with state = Connected { host_and_port; conn; login = None } }
      | Connection_failed ->
        { model with state = Connection_failed }
      | Connected cact ->
        begin match model.state with
        | Connected conn ->
          let conn = apply_connected_action cact ~schedule conn in
          { model with state = Connected conn }
        | _ -> model
        end

    let should_log _ = false
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
          ; Widget.textbox ~id:"connectTo"
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

  let market_table ~players ~(inject : Action.t -> _) (market : Book.t) =
    let market_depth = 3 in
    let nbsp = "\xc2\xa0" in
    let empty_cells =
      List.init market_depth ~f:(fun _ ->
        Node.td [] [Node.text nbsp])
    in
    let input_row ~dir =
      let dir_s = Dir.to_string dir in
      let id symbol = "order" ^ dir_s ^ Card.Suit.name symbol in
      let keys =
        Dir.fold dir
          ~sell:["Q"; "W"; "E"; "R"]
          ~buy:["A"; "S"; "D"; "F"]
      in
      Node.tr [Attr.id ("order" ^ dir_s); Attr.class_ dir_s]
        (List.map2_exn Card.Suit.all keys ~f:(fun symbol key ->
          Node.td [] [
            Widget.textbox ~id:(id symbol) ~placeholder:key
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
              let class_ = Player_id.class_ player.pers.id in
              Node.td [] [
                Node.span
                  [Attr.class_ class_]
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

  let cxl_by_id ~inject =
    [ Widget.textbox ~id:"cxl" ~placeholder:"X" ~f:(fun oid ->
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
        [Attr.class_ "name me"]
    in
    infoboxes
      [infobox ~pos:Me ~name:login_textbox ~score:None ~info:[]]

  let player_infobox ~pos ~(pers : Player.Persistent.t) ~ranking ~info =
    let score =
      Node.span
        [Attr.class_ (sprintf "score %s" ranking)]
        [Node.text (Price.to_string pers.score)]
    in
    let name =
      let class_ = sprintf "name %s" (Player_id.class_ pers.id) in
      Node.span
        [ Attr.class_ class_ ]
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

  let history ~players ~messages =
    let nodes_of_message : Message.t -> _ =
      function
      | Chat (who, msg) ->
        let player =
          Map.find players who
          |> Option.value ~default:Player.nobody
        in
        [ Node.span
            [Attr.class_ (Player_id.class_ player.pers.id)]
            [Node.text (Username.to_string player.pers.username)]
        ; Node.text ": "
        ; Node.text msg
        ]
      | Order_reject reject ->
        [ Node.text
            (Protocol.Order.sexp_of_error reject |> Sexp.to_string)
        ]
      | Cancel_reject reject ->
        [ Node.text
            (Protocol.Cancel.sexp_of_error reject |> Sexp.to_string)
        ]
    in
    Node.ul [Attr.id "history"]
      (List.map (Fqueue.to_list messages) ~f:(fun msg ->
        Node.li [] (nodes_of_message msg)))

  let cmdline (model : Model.t) =
    let this_id = "cmdline" in
    Widget.textbox ~id:this_id
      ~f:(fun msg ->
        begin match model.state with
        | Connected { conn; _ } ->
          don't_wait_for begin
            Rpc.Rpc.dispatch_exn Protocol.Chat.rpc conn msg
            >>| function
            | Error `Login_first | Ok () -> ()
          end
        | _ -> ()
        end;
        Event.Ignore)
      [ Attr.property "disabled"
          (Js.Unsafe.inject (Js.bool
            (match model.state with | Connected _ -> false | _ -> true)))
      ]

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
    let open Node in
    body [] [div [Attr.id "container"]
      [ status_line ~inject model.state
      ; table [Attr.id "exchange"]
        [ tr []
          [ td [] [market_table ~players ~inject market]
          ; td [] [tape_table ~my_username ~players market trades]
          ]
        ; tr []
          [ td [] [market_help]
          ; td [Attr.id "cxlcontainer"] (cxl_by_id ~inject)
          ]
        ]
      ; infoboxes
      ; div [Attr.id "historycmd"]
        [ history ~players ~messages:model.messages
        ; cmdline model
        ]
      ]
    ]

  let on_startup ~schedule _model =
    Option.iter (List.Assoc.find Url.Current.arguments "autoconnect")
      ~f:(fun v -> schedule (Action.Start_connecting (parse_host_and_port v)))

  let on_display ~schedule:_ ~(old : Model.t) (new_ : Model.t) =
    match old.state, new_.state with
    | Connecting _, Connected _ ->
      focus_input ~element_id:Ids.login
    | Connected { login = None; _ }, Connected { login = Some _; _ } ->
      (* would like to focus ready button here, but buttonElement doesn't
         seem to have a focus method *)
      ()
    | _ -> ()

  let update_visibility model = model
end

let () =
  Start_app.simple
    ~initial_state:App.Model.initial
    (module App)
