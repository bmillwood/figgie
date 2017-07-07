open Core_kernel.Std
open Incr_dom
open Vdom

open Figgie
open Market

module Message = struct
  type t =
    | Connected_to_server of Host_and_port.t
    | Disconnected_from_server
    | Chat of { username : Username.t; is_me : bool; msg : string }
    | Chat_failed of [ `Chat_disabled | `Not_logged_in ]
    | Player_room_event of
        { username : Username.t
        ; room_id : Lobby.Room.Id.t option
        ; event : Lobby.Room.Update.User_event.t
        }
    | Player_lobby_event of
        { username : Username.t
        ; event : Lobby.Update.User_event.t
        }
    | Joined_room of Lobby.Room.Id.t
    | New_round
    | Round_over of Lobby.Room.Update.Round_results.t
    | Order_reject of Order.t * Protocol.Order.error
    | Cancel_reject of [ `All | `Id of Order.Id.t ] * Protocol.Cancel.error
    [@@deriving sexp_of]
end

module Model = struct
  type t =
    { messages  : Message.t Fqueue.t
    ; scrolling : Scrolling.Model.t
    }

  let initial =
    { messages = Fqueue.empty
    ; scrolling = Scrolling.Model.create ~id:Id.history
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
  let nodes_of_message : Message.t -> _ =
    let horizontal_rule = Node.create "hr" [] [] in
    let status nodes = [Node.li [Attr.class_ "status"] nodes] in
    let error  nodes = [Node.li [Attr.classes ["status"; "error"]] nodes] in
    let simple mk fmt = ksprintf (fun s -> mk [Node.text s]) fmt in
    function
    | Connected_to_server where ->
      simple status !"Connected to %{Host_and_port}" where
    | Disconnected_from_server ->
      error
        [ Node.text "Disconnected"
        ; horizontal_rule
        ]
    | Chat { username; is_me; msg } ->
      [ Node.li []
          [ Hash_colour.username_span ~is_me username
          ; Node.text ": "
          ; Node.text msg
          ]
      ]
    | Chat_failed `Chat_disabled ->
      error [ Node.text "Chat system administratively disabled" ]
    | Chat_failed `Not_logged_in ->
      error [ Node.text "Must log in to chat" ]
    | Player_room_event { username; room_id; event } ->
      let message_parts =
        match event with
        | Joined ->
          Some ("joined", " ")
        | Observer_became_omniscient ->
          Some ("sees all things", " in ")
        | Observer_started_playing { in_seat = _ } ->
          Some ("is playing", " in ")
        | Player_ready _ ->
          None
        | Disconnected ->
          Some ("disconnected", " from ")
      in
      begin match message_parts with
      | None -> []
      | Some (without_room, with_room) ->
        let msg =
          match room_id with
          | None -> without_room
          | Some id -> without_room ^ with_room ^ Lobby.Room.Id.to_string id
        in
        simple status !"%{Username} %s" username msg
      end
    | Player_lobby_event { username; event } ->
      let verb =
        match event with
        | Connected    -> "connected"
        | Disconnected -> "disconnected"
      in
      simple status !"%{Username} %s" username verb
    | Joined_room room_id ->
      status
        [ horizontal_rule
        ; Node.text (sprintf !"Joined %{Lobby.Room.Id}" room_id)
        ]
    | New_round ->
      simple status "Everyone's ready: game is starting!"
    | Round_over results ->
      status
        [ Node.text "Time's up! The gold suit was "
        ; Node.span [Attr.classes [Card.Suit.name results.gold; "gold"]]
          [ Style.suit_span ~gold:None results.gold
          ; Node.text " "
          ; Node.text (Card.Suit.name results.gold)
          ]
        ; horizontal_rule
        ]
    | Order_reject (_order, reject) ->
      simple error !"%{sexp:Protocol.Order.error}" reject
    | Cancel_reject (oid_or_all, reject) ->
      let order_spec =
        match oid_or_all with
        | `All -> "all orders"
        | `Id i -> sprintf !"order %{Order.Id}" i
      in
      let reject_reason =
        match reject with
        | #Protocol.not_playing -> "not playing a game"
        | `No_such_order -> "order not found (already cancelled or filled?)"
      in
      simple error "Can't cancel %s: %s" order_spec reject_reason
  in
  Node.div [Id.attr Id.chat]
    [ Node.ul
        [ Id.attr Id.history
        ; Scrolling.on_scroll t.scrolling
            ~inject:(fun scroll -> inject (Scroll_chat scroll))
        ]
        (List.concat_map (Fqueue.to_list t.messages) ~f:nodes_of_message)
    ; Widget.textbox ~id:Id.cmdline
        ~disabled:(not is_connected)
        ~on_submit:(fun msg -> inject (Send_chat msg))
        ()
    ]

let on_display ~(old : Model.t) (new_ : Model.t) ~schedule_scroll =
  ignore old;
  Scrolling.on_display new_.scrolling ~schedule:schedule_scroll;
