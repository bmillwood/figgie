open Core_kernel.Std
open Incr_dom
open Vdom

open Market

module Username_with_class = struct
  type t = Username.t * string [@@deriving sexp_of]
  
  let span (u, c) =
    Node.span [Attr.class_ c] [Node.text (Username.to_string u)]
end

module Message = struct
  type t =
    | New_round
    | Round_over of Protocol.Round_results.t
    | Order_reject of Order.t * Protocol.Order.error
    | Cancel_reject of [ `All | `Id of Order.Id.t ] * Protocol.Cancel.error
    | Chat of Username_with_class.t * string
    [@@deriving sexp_of]
end

let history_id = "history"

module Model = struct
  type t =
    { messages  : Message.t Fqueue.t
    ; scrolling : Scrolling.Model.t
    }

  let initial =
    { messages = Fqueue.empty
    ; scrolling = Scrolling.Model.create ~id:history_id
    }

  let add_message t message =
    { t with messages = Fqueue.enqueue t.messages message }
end

module Action = struct
  type t =
    | Send_chat of string
    | Scroll_chat of Scrolling.Action.t
    [@@deriving sexp_of]
end

let apply_scrolling_action (t : Model.t) (act : Scrolling.Action.t) =
  { t with scrolling = Scrolling.apply_action t.scrolling act }

let view (t : Model.t) ~is_connected ~(inject : Action.t -> _) =
  let attrs_of_message : Message.t -> _ =
    function
    | Chat _ -> []
    | _ -> [Attr.class_ "status"]
  in
  let nodes_of_message : Message.t -> _ =
    function
    | New_round -> [ Node.text "Everyone's ready: game is starting!" ]
    | Round_over results ->
      [ Node.text "Time's up! The gold suit was "
      ; Node.span [Attr.classes [Card.Suit.name results.gold; "gold"]]
        [ Node.text (Card.Suit.to_utf8 results.gold)
        ; Node.text " "
        ; Node.text (Card.Suit.name results.gold)
        ]
      ; Node.create "hr" [] []
      ]
    | Order_reject (_order, reject) ->
      [ Node.text
          (Protocol.Order.sexp_of_error reject |> Sexp.to_string)
      ]
    | Cancel_reject (_oid_or_all, reject) ->
      [ Node.text
          (Protocol.Cancel.sexp_of_error reject |> Sexp.to_string)
      ]
    | Chat (who, msg) ->
      [ Username_with_class.span who
      ; Node.text ": "
      ; Node.text msg
      ]
  in
  Node.div [Attr.id "historycmd"]
    [ Node.ul
        [ Attr.id history_id
        ; Scrolling.on_scroll t.scrolling
            ~inject:(fun scroll -> inject (Scroll_chat scroll))
        ]
        (List.map (Fqueue.to_list t.messages) ~f:(fun msg ->
          Node.li (attrs_of_message msg) (nodes_of_message msg)))
    ; Widget.textbox ~id:Ids.cmdline
        ~disabled:(not is_connected)
        ~on_submit:(fun msg -> inject (Send_chat msg))
        ()
    ]

let on_display ~(old : Model.t) (new_ : Model.t) ~schedule_scroll =
  ignore old;
  Scrolling.on_display new_.scrolling ~schedule:schedule_scroll;
