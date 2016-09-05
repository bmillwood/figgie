open Core_kernel.Std
open Async_kernel.Std
open Async_rpc_kernel.Std
open Incr_dom.Std

module Figgie_web = struct
  module Connection_status = struct
    type t =
      | Connecting
      | Failed_to_connect
      | Connected
      | Disconnected
      [@@deriving sexp]
  end

  module Model = struct
    type t = {
      connection_status : Connection_status.t;
      broadcasts        : Protocol.Broadcast.t Fqueue.t;
      hands             : Market.Size.t Card.Hand.t Username.Map.t;
      market            : Market.Book.t;
      gold              : Card.Suit.t option;
    }

    let initial =
      { connection_status = Connecting
      ; broadcasts = Fqueue.empty
      ; hands = Username.Map.empty
      ; market = Market.Book.empty
      ; gold = None
      }

    let max_broadcasts = 1000
  end

  module Action = struct
    type t =
      | Set_connection_status of Connection_status.t
      | Set_hands of Market.Size.t Card.Hand.t Username.Map.t
      | Set_market of Market.Book.t
      | Set_gold of Card.Suit.t
      | Add_broadcast of Protocol.Broadcast.t
      [@@deriving sexp]

    let apply action ~schedule:_ (model : Model.t) =
      match action with
      | Set_connection_status connection_status ->
        { model with connection_status }
      | Set_hands hands ->
        { model with hands }
      | Set_market market ->
        { model with market }
      | Set_gold gold ->
        { model with gold = Some gold }
      | Add_broadcast broadcast ->
        let old_broadcasts =
          if Fqueue.length model.broadcasts = Model.max_broadcasts
          then Fqueue.discard_exn model.broadcasts
          else model.broadcasts
        in
        { model with broadcasts = Fqueue.enqueue old_broadcasts broadcast }

    let should_log _ = false
  end

  let status_span (connection_status : Connection_status.t) =
    let node ?(fg="black") ?(bg="transparent") text =
      Vdom.Node.span [Vdom.Attr.style ["color", fg; "background-color", bg]]
        [Vdom.Node.text text]
    in
    match connection_status with
    | Connecting        -> node ~bg:"yellow" "Connecting"
    | Failed_to_connect -> node ~fg:"white" ~bg:"red" "Failed to connect"
    | Connected         -> node ~fg:"white" ~bg:"green" "Connected"
    | Disconnected      -> node ~fg:"white" ~bg:"red" "Disconnected"

  let market_display market =
    let best_price ~symbol ~side =
      let of_order (order : Market.Order.t) =
        Market.Price.to_string order.price
      in
      Market.Per_symbol.get market ~symbol
      |> Market.Dirpair.get ~dir:side
      |> List.hd
      |> Option.value_map ~default:"" ~f:of_order
    in
    let open Vdom.Node in
    let row ?(tr=tr) ?(td=td) cells =
      tr [] (List.map cells ~f:(fun data -> td [] [text data]))
    in
    let sym_row symbol =
      row
        [ Card.Suit.name symbol
        ; best_price ~symbol ~side:Buy
        ; best_price ~symbol ~side:Sell
        ]
    in
    table []
      (row ~tr:thead ~td:th ["Symbol"; "Bid"; "Ask"]
      :: List.map Card.Suit.all ~f:sym_row)

  let hands_display hands ~gold =
    let utf8_of_suit : Card.Suit.t -> string = function
      | Spades -> "\xe2\x99\xa0"
      | Hearts -> "\xe2\x99\xa5"
      | Diamonds -> "\xe2\x99\xa6"
      | Clubs -> "\xe2\x99\xa3"
    in
    let node_of_suit suit num_of_this_suit =
      let text_node =
        Vdom.Node.text
          (List.init (Market.Size.to_int num_of_this_suit)
            ~f:(fun _ -> utf8_of_suit suit)
          |> String.concat)
      in
      let maybe_gold =
        if Option.exists gold ~f:(Card.Suit.equal suit)
        then [Vdom.Attr.class_ "gold"] else []
      in
      Vdom.Node.span (Vdom.Attr.id (Card.Suit.name suit) :: maybe_gold)
        [text_node]
    in
    let player_hands =
      Map.map hands ~f:(fun hand ->
        Card.Hand.foldi hand ~init:[] ~f:(fun suit acc num_of_this_suit ->
          node_of_suit suit num_of_this_suit :: acc)
        |> List.rev)
    in
    let open Vdom.Node in
    table []
      (thead [] [th [] [text "Player"]; th [] [text "Cards"]]
      :: Map.fold player_hands ~init:[]
        ~f:(fun ~key:username ~data:hand acc ->
          tr []
            [ td [] [text (Username.to_string username)]
            ; td [] hand
            ] :: acc))

  let describe_order_cancel ~cancel (order : Market.Order.t) =
    match order.dir with
    | Buy ->
      sprintf !"%{Username} %s %{Market.Price} bid for %{Market.Size} %s"
        order.owner
        (if cancel then "cancels their" else "is")
        order.price
        order.size
        (Card.Suit.name order.symbol)
    | Sell ->
      sprintf !"%{Username} %s %{Market.Size} %s at %{Market.Price}"
        order.owner
        (if cancel then "cancels their offer of" else "has")
        order.size
        (Card.Suit.name order.symbol)
        order.price

  let items_of_broadcast (bc : Protocol.Broadcast.t) =
    match bc with
    | Player_joined u -> [sprintf !"%{Username} joined" u]
    | Chat (u, m)     -> [sprintf !"<%{Username}> %S" u m]
    | Waiting_for _   -> []
    | Exec (order, exec) ->
      let fill_to_string (filled_order : Market.Order.t) ~filled_by =
        sprintf
          !"%s %(%s%s%) %s for %s"
          (Username.to_string order.owner)
          (Market.Dir.fold order.dir
            ~buy: (format_of_string "bought %s %s from")
            ~sell:(format_of_string "sold %s %s to"))
          (Market.Size.to_string filled_by)
          (Card.Suit.name order.symbol)
          (Username.to_string filled_order.owner)
          (Market.Price.to_string filled_order.price)
      in
      let of_option opt ~f = Option.to_list (Option.map opt ~f) in
      List.concat
        [ List.map exec.fully_filled ~f:(fun filled_order ->
            fill_to_string filled_order ~filled_by:filled_order.size)
        ; of_option exec.partially_filled ~f:(fun pf ->
            fill_to_string pf.original_order ~filled_by:pf.filled_by)
        ; of_option exec.posted ~f:(fun order ->
            describe_order_cancel ~cancel:false order)
        ]
    | Out order -> [describe_order_cancel ~cancel:true order]
    | Round_over _results -> ["Round over!"]

  let broadcasts_list broadcasts =
    List.concat_map (Fqueue.to_list broadcasts) ~f:items_of_broadcast
    |> List.map ~f:(fun item -> Vdom.Node.li [] [Vdom.Node.text item])
    |> fun items -> List.drop items (List.length items - 20)
    |> Vdom.Node.ul []

  let view (incr_model : Model.t Incr.t) ~inject:_ =
    let open Incr.Let_syntax in
    let%map model = incr_model in
    Vdom.Node.body []
      [ Vdom.Node.p  [] [status_span model.connection_status]
      ; market_display model.market
      ; hands_display model.hands ~gold:model.gold
      ; broadcasts_list model.broadcasts
      ]

  let on_startup ~schedule _ =
    don't_wait_for begin
      let open Action in
      let handle_update : Protocol.Observer_update.t -> unit =
        function
        | Broadcast broadcast -> schedule (Add_broadcast broadcast)
        | Hands hands -> schedule (Set_hands hands)
        | Market market -> schedule (Set_market market)
        | Gold gold -> schedule (Set_gold gold)
      in
      schedule (Set_connection_status Connecting);
      Websocket_rpc_transport.connect
        (Host_and_port.create ~host:"localhost" ~port:20406)
      >>= function
      | Error () ->
        schedule (Set_connection_status Failed_to_connect);
        Deferred.unit
      | Ok transport ->
        Rpc.Connection.with_close
          ~connection_state:(fun _ -> ())
          transport
          ~on_handshake_error:`Raise
          ~dispatch_queries:(fun conn ->
            schedule (Set_connection_status Connected);
            Rpc.Pipe_rpc.dispatch_iter
              Protocol.Get_observer_updates.rpc conn ()
              ~f:(function
                | Update update -> handle_update update; Continue
                | Closed _ -> Continue)
            >>| ok_exn
            >>= function
            | Error `Already_logged_in ->
              failwith "Server says I'm already logged in"
            | Ok _pipe_id -> Deferred.never ()
          )
        >>| fun () ->
        schedule (Set_connection_status Disconnected)
    end

  let on_display ~schedule:_ ~old:_ _new = ()

  let update_visibility model = model
end

let () =
  Start_app.simple
    ~initial_state:Figgie_web.Model.initial
    (module Figgie_web)
