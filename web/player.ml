open Core_kernel.Std
open Async_kernel.Std
open Async_rpc_kernel.Std
open Incr_dom.Std

module Player_app = struct
  module Status = struct
    type t =
      | Connecting
      | Connected of { conn : Rpc.Connection.t; is_ready : bool }
      | Failed_to_connect
      | Disconnected
      [@@deriving sexp_of]
  end

  module Message = struct
    type t =
      | Broadcast of Protocol.Broadcast.t
      | Log of string
      [@@deriving sexp_of]
  end

  module Other_player = struct
    type t = {
      id : int;
      hand : Market.Size.t Card.Hand.t;
    }

    let class_ t = "them" ^ Int.to_string t.id
  end

  module Model = struct
    type t = {
      username   : Username.t option;
      others     : Other_player.t Username.Map.t;
      messages   : Message.t Fqueue.t;
      trades     : (Market.Order.t * Market.Cpty.t) Fqueue.t;
      next_order : Market.Order.Id.t;
      market     : Market.Book.t;
      hand       : Market.Size.t Card.Hand.t;
      score      : Market.Price.t;
      status     : Status.t;
    }

    let initial =
      { username   = None
      ; others     = Username.Map.empty
      ; messages   = Fqueue.empty
      ; trades     = Fqueue.empty
      ; next_order = Market.Order.Id.zero
      ; market     = Market.Book.empty
      ; hand       = Card.Hand.create_all Market.Size.zero
      ; score      = Market.Price.zero
      ; status     = Disconnected
      }
  end

  module Action = struct
    type t =
      | Login of Username.t
      | Player_joined of Username.t
      | Message of Message.t
      | Market of Market.Book.t
      | Hand of Market.Size.t Card.Hand.t
      | Trade of Market.Order.t * Market.Cpty.t
      | Score of Market.Price.t
      | Status of Status.t
      | Send_order of {
          symbol : Card.Suit.t;
          dir    : Market.Dir.t; 
          price  : Market.Price.t;
        }
      | Set_ready of bool
      [@@deriving sexp_of]

    let apply t ~schedule (model : Model.t) =
      let if_connected ~f =
        match model.status with
        | Connected { conn; _ } -> f conn
        | _ -> ()
      in
      match t with
      | Login username -> { model with username = Some username }
      | Market market  -> { model with market }
      | Hand hand      -> { model with hand }
      | Score score    -> { model with score }
      | Status status  -> { model with status }
      | Player_joined other ->
        let id =
          let rec loop n =
            if Map.exists model.others ~f:(fun other -> n = other.id)
            then loop (n + 1)
            else n
          in
          loop 1
        in
        let hand = Card.Hand.create_all Market.Size.zero in
        let others = Map.add model.others ~key:other ~data:{ id; hand } in
        { model with others }
      | Trade (trade, with_) ->
        { model with trades = Fqueue.enqueue model.trades (trade, with_) }
      | Message m ->
        { model with messages = Fqueue.enqueue model.messages m }
      | Set_ready is_ready ->
        if_connected ~f:(fun conn ->
          schedule (Status (Connected { conn; is_ready }));
          don't_wait_for begin
            Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc conn is_ready
            |> Deferred.ignore;
          end);
        model
      | Send_order { symbol; dir; price } ->
        let order =
          { Market.Order.owner = Username.of_string "bmillwood"
          ; id = model.next_order
          ; symbol; dir; price
          ; size = Market.Size.of_int 1
          } 
        in
        if_connected ~f:(fun conn ->
          don't_wait_for begin
            Rpc.Rpc.dispatch_exn Protocol.Order.rpc conn order
            >>| function
            | Ok `Ack -> ()
            | Error reject ->
              schedule (Message (Log (Sexp.to_string [%sexp (reject : Protocol.Order.error)])))
          end);
        { model with next_order = Market.Order.Id.next model.next_order }

    let should_log _ = false
  end

  let status_line ~(inject : Action.t -> _) (status : Status.t) =
    let ready_button is_ready =
      Vdom.Node.button
        [ Vdom.Attr.on_click (fun _ev -> inject (Set_ready (not is_ready)))
        ; Vdom.Attr.id "readybutton"
        ]
        [ Vdom.Node.text (if is_ready then "Not ready" else "Ready") ]
    in
    let status_text, ready_button =
      match status with
      | Connecting -> "Connecting", None
      | Failed_to_connect -> "Failed_to_connect", None
      | Connected { conn = _; is_ready } ->
        "Connected", Some (ready_button is_ready)
      | Disconnected -> "Disconnected", None
    in
    List.filter_opt
      [ Some (Vdom.Node.text status_text)
      ; ready_button
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
          inject (Send_order { symbol; dir; price })
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

  let trades_table ~others trades =
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
              | Some other -> [Attr.class_ (Other_player.class_ other)]
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
    let open Vdom in
    let open Node in
    body [] [div [Attr.id "container"]
      [ status_line ~inject model.status
      ; div [Attr.id "exchange"]
        [ market_table ~inject model.market
        ; trades_table ~others:model.others model.trades
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
        | Hand hand     -> schedule (Action.Hand hand)
        | Market market -> schedule (Action.Market market)
        | Broadcast (Exec (order, exec)) ->
          List.iter exec.fully_filled ~f:(fun filled_order ->
            schedule (Trade
              ( { order with size = filled_order.size }
              , filled_order.owner
              )));
          Option.iter exec.partially_filled ~f:(fun partial_fill ->
            schedule (Trade
              ( { order with size = partial_fill.filled_by }
              , partial_fill.original_order.owner
              )));
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
          schedule (Player_joined username)
        | Broadcast b -> schedule (Message (Broadcast b))
      in
      schedule (Status Connecting);
      Websocket_rpc_transport.connect
        (Host_and_port.create ~host:"localhost" ~port:20406)
      >>= function
      | Error () ->
        schedule (Status Failed_to_connect);
        Deferred.unit
      | Ok transport ->
        Rpc.Connection.with_close
          ~connection_state:(fun _ -> ())
          transport
          ~on_handshake_error:`Raise
          ~dispatch_queries:(fun conn ->
            schedule (Status (Connected { conn; is_ready = false }));
            Rpc.Pipe_rpc.dispatch_iter
              Protocol.Login.rpc conn (Username.of_string "bmillwood")
              ~f:(function
                | Update update -> handle_update conn update; Continue
                | Closed _ -> schedule (Status Disconnected); Continue)
            >>| ok_exn
            >>= function
            | Error error ->
              Error.raise_s [%message
                "login failed"
                  (error : Protocol.Login.error)
              ]
            | Ok _pipe_id -> Deferred.never ()
          )
    end

  let on_display ~schedule:_ ~old:_ _new = ()
  let update_visibility model = model
end

let () =
  Start_app.simple
    ~initial_state:Player_app.Model.initial
    (module Player_app)
