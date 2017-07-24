open Core_kernel.Std
open Async_kernel
open Async_rpc_kernel
open Incr_dom
open Vdom

open Figgie
open Market

module Model = struct
  module Highlight = struct
    module State = struct
      type t = | No | Toggling | Yes
    end
    open State

    type t = State.t array Dirpair.t Per_symbol.t

    let empty : t = Card.Hand.create_all (Dirpair.create_both [| |])

    let toggle_off t ~symbol ~dir ~depth =
      Card.Hand.modify t ~suit:symbol
        ~f:(Dirpair.modify ~dir ~f:(fun a ->
            if depth > Array.length a then (
              Array.init depth
                ~f:(fun _ -> Toggling)
            ) else (
              let a = Array.copy a in
              for i = 0 to depth - 1 do
                a.(i) <- Toggling
              done;
              a
            )
          ))

    let restore t =
      let toggle_to_yes =
        function
        | Toggling | Yes -> Yes
        | No -> No
      in
      Card.Hand.map t ~f:(Dirpair.map ~f:(Array.map ~f:toggle_to_yes))
  end

  type t =
    { market        : Book.t
    ; trades        : (Order.t * Cpty.t) Fqueue.t
    ; trades_scroll : Scrolling.Model.t
    ; next_order    : Order.Id.t
    ; pending_ack   : Order.Id.Set.t Dirpair.t Card.Hand.t
    ; highlight     : Highlight.t
    }

  let empty =
    { market        = Book.empty
    ; trades        = Fqueue.empty
    ; trades_scroll = Scrolling.Model.create ~id:Id.tape
    ; next_order    = Order.Id.zero
    ; pending_ack   =
        Card.Hand.create_all (Dirpair.create_both Order.Id.Set.empty)
    ; highlight     = Highlight.empty
    }
end
open Model

let set_market t ~market = { t with market }

let exec t ~my_name ~(exec : Exec.t) =
  let { Order.owner; id; dir; symbol; _ } = exec.order in
  let fills = Exec.fills exec in
  let trades =
    List.map fills ~f:(fun filled_order ->
        ({ filled_order with owner; id; dir }, filled_order.owner)
      )
    |> List.fold ~init:t.trades ~f:Fqueue.enqueue
  in
  let num_price_levels =
    List.map fills ~f:(fun filled_order -> filled_order.price)
    |> Price.Set.of_list
    |> Set.length
  in
  let pending_ack =
    if Username.equal owner my_name then (
      Card.Hand.modify
        t.pending_ack
        ~suit:symbol
        ~f:(Dirpair.modify ~dir ~f:(fun pending -> Set.remove pending id))
    ) else (
      t.pending_ack
    )
  in
  let highlight =
    Model.Highlight.toggle_off t.highlight
      ~symbol ~dir:(Dir.other dir) ~depth:num_price_levels
  in
  { t with trades; pending_ack; highlight }

module Cancel_scope = struct
  (* constructor {All,By_id} is never used to build values. *)
  [@@@ocaml.warning "-37"]

  type t =
    | All
    | By_id of Order.Id.t
    | By_symbol_side of { symbol : Symbol.t; dir : Dir.t }
    [@@deriving sexp_of]
end

module Action = struct
  type t =
    | Send_order of
        { symbol : Card.Suit.t
        ; dir    : Dir.t
        ; price  : Price.t
        }
    | Send_cancel of Cancel_scope.t
    | Scroll_trades of Scrolling.Action.t
    | Reset_highlight
    [@@deriving sexp_of]
end
open Action

let cancel_reject_message ~oid ~reject =
  Chat.Message.(simple error)
    !"Couldn't cancel #%{Order.Id}: %{sexp:Protocol.Cancel.error}"
    oid reject

let apply_action action model ~my_name ~conn
  ~(add_message : Chat.Message.t -> unit)
  =
  match action with
  | Send_order { symbol; dir; price } ->
    let id = model.next_order in
    let next_order = Order.Id.next id in
    let order =
      { Order.owner = my_name
      ; id = model.next_order
      ; symbol; dir; price
      ; size = Size.of_int 1
      }
    in
    don't_wait_for begin
      Rpc.Rpc.dispatch_exn Protocol.Order.rpc conn order
      >>| function
      | Ok `Ack -> ()
      | Error reject ->
        add_message (
          Chat.Message.(simple error)
            !"%{sexp:Protocol.Order.error}" reject
        )
    end;
    let pending_ack =
      Card.Hand.modify model.pending_ack ~suit:symbol
        ~f:(Dirpair.modify ~dir ~f:(fun pending -> Set.add pending id))
    in
    { model with next_order; pending_ack }
  | Send_cancel (By_id oid) ->
    don't_wait_for begin
      Rpc.Rpc.dispatch_exn Protocol.Cancel.rpc conn oid
      >>| function
      | Ok `Ack -> ()
      | Error reject ->
        add_message (cancel_reject_message ~oid ~reject)
    end;
    model
  | Send_cancel (By_symbol_side { symbol; dir }) ->
    don't_wait_for begin
      let pending_order_ids =
        Card.Hand.get model.pending_ack ~suit:symbol
        |> Dirpair.get ~dir
        |> Set.to_list
      in
      let live_order_ids =
        Card.Hand.get model.market ~suit:symbol
        |> Dirpair.get ~dir
        |> List.filter_map ~f:(fun order ->
            Option.some_if (Cpty.equal order.owner my_name) order.id
          )
      in
      Deferred.List.iter
        (pending_order_ids @ live_order_ids)
        ~how:`Parallel
        ~f:(fun oid ->
            Rpc.Rpc.dispatch_exn Protocol.Cancel.rpc conn oid
            >>| function
            | Ok `Ack -> ()
            | Error reject ->
              add_message (cancel_reject_message ~oid ~reject)
          )
    end;
    let pending_ack =
      Card.Hand.modify model.pending_ack ~suit:symbol
        ~f:(Dirpair.set ~dir ~to_:Order.Id.Set.empty)
    in
    { model with pending_ack }
  | Send_cancel All ->
    don't_wait_for begin
      Rpc.Rpc.dispatch_exn Protocol.Cancel_all.rpc conn ()
      >>| function
      | Ok `Ack -> ()
      | Error reject ->
        add_message (
          Chat.Message.(simple error)
            !"Can't cancel orders: %{sexp:Protocol.not_playing}"
            reject
        )
    end;
    model
  | Scroll_trades scract ->
    let trades_scroll = Scrolling.apply_action model.trades_scroll scract in
    { model with trades_scroll }
  | Reset_highlight ->
    { model with highlight = Model.Highlight.restore model.highlight }

let order_entry
    ~(market : Book.t) ~(inject : Action.t -> _)
    ~can_send_orders ~symbol ~dir
  =
  let id = Id.order ~dir ~suit:symbol in
  let hotkey = Hotkeys.Global.lookup_id id in
  let placeholder = Hotkeys.Global.placeholder_of_id id in
  let book = Card.Hand.get market ~suit:symbol in
  Widget.textbox ~id ?placeholder
    ~disabled:(not can_send_orders)
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
    ~on_submit:(fun order_spec ->
      let actions =
        String.to_list order_spec
        |> List.group ~break:(fun c1 c2 ->
            not (Bool.equal (Char.is_digit c1) (Char.is_digit c2))
          )
        |> List.map ~f:String.of_char_list
      in
      Event.Many (List.map actions ~f:(fun action ->
        if String.Caseless.equal action "x"
        then begin
          inject (Send_cancel (By_symbol_side { symbol; dir }))
        end else match Price.of_string action with
        | exception _ -> Event.Ignore
        | price ->
          inject (Send_order { symbol; dir; price }))
      ))
    ()

let market_table
  ~shortener ~gold ~can_send_orders ~my_name ~(inject : Action.t -> _)
  ~(highlight : Model.Highlight.t)
  (market : Book.t)
  =
  let market_depth = 3 in
  let nbsp = "\xc2\xa0" in
  let empty_cell_contents =
    List.init market_depth ~f:(fun _ ->
      [Node.text nbsp])
  in
  let input_row ~dir =
    let dir_s = Dir.to_string dir in
    Node.tr [Id.attr (Id.order_row ~dir); Attr.class_ dir_s]
      (List.map Card.Suit.all ~f:(fun symbol ->
          Node.td []
            [order_entry ~market ~can_send_orders ~inject ~symbol ~dir]
        )
      )
  in
  let cells ~dir =
    let shortname_s username =
      Username.Shortener.short shortener username
      |> Username.to_string
    in
    List.map Card.Suit.all ~f:(fun symbol ->
        let halfbook = Dirpair.get (Per_symbol.get market    ~symbol) ~dir in
        let hili     = Dirpair.get (Per_symbol.get highlight ~symbol) ~dir in
        let cell_contents =
          List.map (List.take halfbook market_depth)
            ~f:(fun order ->
                let is_me = Username.equal my_name order.owner in
                let span ~attrs text =
                  Node.span
                    (Attr.style
                       (Style.Name.style ~is_me order.owner)
                     :: attrs)
                    [Node.text text]
                in
                [ span ~attrs:[Attr.class_ "owner"]
                    (shortname_s order.owner)
                ; span ~attrs:[]
                    (Price.to_string order.price)
                ]
              )
        in
        List.mapi
          (List.take (cell_contents @ empty_cell_contents) market_depth)
          ~f:(fun i contents ->
              let attrs =
                if i >= Array.length hili then (
                  []
                ) else (
                  match hili.(i) with
                  | Toggling | No -> []
                  | Yes ->
                    [Attr.class_ (Dir.fold dir ~buy:"bought" ~sell:"sold")]
                )
              in
              Node.td attrs contents
            )
      )
    |> List.transpose_exn
    |> Dir.fold dir ~buy:Fn.id ~sell:List.rev
    |> List.map ~f:(fun row ->
        Node.tr [Attr.class_ (Dir.to_string dir)] row)
  in
  Node.table [Id.attr Id.market]
    (List.concat
      [ cells ~dir:Sell
      ; [input_row ~dir:Sell]
      ; [Node.tr [Id.attr Id.suits_row]
          (List.map Card.Suit.all ~f:(fun suit ->
            Node.td [] [Style.suit_span ~gold suit]
          ))]
      ; [input_row ~dir:Buy]
      ; cells ~dir:Buy
      ])

let tape_table ~my_name ~gold trades =
  let row_of_order ~traded_with (trade : Order.t) =
    let person_td username =
      Style.Name.span
        ~is_me:(Username.equal username my_name)
        username
    in
    [ person_td trade.owner
    ; Node.td [Attr.class_ (Dir.to_string trade.dir)]
        [Node.text (Dir.fold trade.dir ~buy:"B" ~sell:"S")]
    ; begin let size_n =
        if Size.equal trade.size (Size.of_int 1)
        then []
        else [Node.text (Size.to_string trade.size)]
      in
      Node.td []
        (size_n
         @ [Style.suit_span ~gold trade.symbol]
        )
      end
    ; Node.td [Attr.class_ "price"]
        [Node.text (Price.to_string trade.price)]
    ; person_td traded_with
    ]
    |> Node.tr []
  in
  let trades =
    Fqueue.to_list trades
    |> List.map ~f:(fun ((traded : Order.t), with_) ->
        row_of_order ~traded_with:with_ traded)
  in
  Node.table [Id.attr Id.tape] trades

let view model ~my_name ~shortener ~gold ~can_send_orders ~inject =
  Node.table [Id.attr Id.exchange]
    [ Node.tr []
      [ Node.td []
          [ market_table
              ~my_name ~shortener ~gold ~can_send_orders ~inject
              ~highlight:model.highlight
              model.market
          ]
      ; Node.td []
          [tape_table ~my_name ~gold model.trades]
      ]
    ]

let on_display ~old:_ model ~schedule =
  Scrolling.on_display model.trades_scroll
    ~schedule:(fun act -> schedule (Scroll_trades act));
  begin
    let needs_reset =
      Card.Hand.exists model.highlight ~f:(fun dp ->
          let is_toggling : Model.Highlight.State.t -> bool =
            function
            | Toggling -> true
            | Yes | No -> false
          in
          List.exists [dp.buy; dp.sell] ~f:(Array.exists ~f:is_toggling)
        )
    in
    if needs_reset then (
      schedule Reset_highlight
    )
  end
