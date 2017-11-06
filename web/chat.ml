open Core_kernel.Std
open Incr_dom
open Vdom

open Figgie

module Message = struct
  type t = Node.t

  let sexp_of_t t =
    let html = (Node.to_dom t)##.outerHTML in
    Sexp.Atom (Js.to_string html)

  let horizontal_rule = Node.create "hr" [] []
  let status nodes = Node.li [Attr.class_ "status"] nodes
  let error  nodes = Node.li [Attr.classes ["status"; "error"]] nodes
  let simple mk fmt = ksprintf (fun s -> mk [Node.text s]) fmt

  let chat ~username ~is_me ~msg =
    Node.li []
      [ Style.Name.span ~is_me username
      ; Node.text ": "
      ; Node.text msg
      ]

  let player_event ~username ~room_id ~event =
    let message_parts =
      match (event : Lobby.Room.Update.User_event.t) with
      | Joined { is_bot } ->
        let bot_prefix =
          if is_bot then "(bot) " else ""
        in
        Some (bot_prefix ^ "joined", " ")
      | Observer_became_omniscient ->
        Some ("sees all things", " in ")
      | Observer_started_playing { in_seat = _ } ->
        Some ("is playing", " in ")
      | Player_ready _ ->
        None
      | Disconnected ->
        Some ("disconnected", " from ")
    in
    Option.map message_parts ~f:(fun (without_room, with_room) ->
        let msg =
          match room_id with
          | None -> without_room
          | Some id ->
            without_room ^ with_room ^ Lobby.Room.Id.to_string id
        in
        simple status !"%{Username} %s" username msg
      )
end

module Model = struct
  type t =
    { messages  : Message.t Fqueue.t
    ; scrolling : Scrolling.Model.t
    }

  let initial =
    { messages =
      [ Message.status
          [ Node.text "Figgie ("
          ; Node.a [Attr.href "https://github.com/bmillwood/figgie"] [Node.text "github"]
          ; Node.text ")"
          ]
      ; Message.horizontal_rule
      ]
      |> Fqueue.of_list
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
  Node.div [Id.attr Id.chat]
    [ Node.ul
        [ Id.attr Id.history
        ; Scrolling.on_scroll t.scrolling
            ~inject:(fun scroll -> inject (Scroll_chat scroll))
        ]
        (Fqueue.to_list t.messages)
    ; Widget.textbox ~id:Id.cmdline
        ~disabled:(not is_connected)
        ~on_submit:(fun msg -> inject (Send_chat msg))
        ()
    ]

let on_display ~(old : Model.t) (new_ : Model.t) ~schedule_scroll =
  ignore old;
  Scrolling.on_display new_.scrolling ~schedule:schedule_scroll;
