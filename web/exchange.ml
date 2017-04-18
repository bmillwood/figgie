open Core_kernel.Std
open Incr_dom
open Vdom

open Figgie
open Market

module Cancel_scope = struct
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
    [@@deriving sexp_of]
end

let order_entry ~(market : Book.t) ~(inject : Action.t -> _) ~symbol ~dir =
  let id = Ids.order ~dir ~suit:symbol in
  let hotkey = Hotkeys.Global.lookup_id id in
  let placeholder = Hotkeys.Global.placeholder_of_id id in
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
        inject (Send_cancel (By_symbol_side { symbol; dir }))
      end else match Price.of_string price_s with
      | exception _ -> Event.Ignore
      | price ->
        inject (Send_order { symbol; dir; price }))
    ()

let market_table
  ~players ~my_name ~(inject : Action.t -> _) (market : Book.t)
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
        Node.td [] [order_entry ~market ~inject ~symbol ~dir]))
  in
  let cells ~dir =
    let shortname_s username =
      let everyone = Set.to_list players in
      Username.Shortener.(short (of_list everyone) username)
      |> Username.to_string
    in
    List.map Card.Suit.all ~f:(fun symbol ->
      let halfbook = Dirpair.get (Per_symbol.get market ~symbol) ~dir in
      let cells =
        List.map (List.take halfbook market_depth)
          ~f:(fun order ->
            let is_me = Username.equal my_name order.owner in
            let span ~attrs text =
              Node.span
                (Attr.style
                  (Hash_colour.username_style ~is_me order.owner)
                :: attrs)
                [Node.text text]
            in
            Node.td []
              [ span ~attrs:[Attr.class_ "owner"]
                  (shortname_s order.owner)
              ; span ~attrs:[]
                  (Price.to_string order.price)
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

let tape_table ~my_name trades =
  let row_of_order ~traded_with (trade : Order.t) =
    let person_td username =
      Hash_colour.username_span
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
        @ [ Node.span [Attr.class_ (Card.Suit.name trade.symbol)]
            [Node.text (Card.Suit.to_utf8 trade.symbol)]
          ])
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
  Node.table [Attr.id Ids.tape] trades

let view ~my_name ~market ~trades ~players ~inject =
  Node.table [Attr.id "exchange"]
    [ Node.tr []
      [ Node.td []
          [market_table ~my_name ~players ~inject market]
      ; Node.td []
          [tape_table ~my_name trades]
      ]
    ]
