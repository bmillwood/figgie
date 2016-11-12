open Core_kernel.Std
open Incr_dom.Std
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

module Action = struct
  type t = Send_chat of string
end

let view ~messages ~is_connected ~inject =
  let nodes_of_message : Message.t -> _ =
    function
    | New_round -> [ Node.text "Everyone's ready: game is starting!" ]
    | Round_over results ->
      [ Node.strong [] [Node.text "Time's up!"]
      ; Node.text " The gold suit was "
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
    [ Node.ul [Attr.id "history"]
        (List.map (Fqueue.to_list messages) ~f:(fun msg ->
          Node.li [] (nodes_of_message msg)))
    ; Widget.textbox ~id:Ids.cmdline
        ~disabled:(not is_connected)
        ~on_submit:(fun msg -> inject (Action.Send_chat msg))
        ()
    ]
