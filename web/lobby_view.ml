open Core_kernel.Std
open Incr_dom
open Vdom

open Figgie

module Action = struct
  module Room_action = struct
    type t =
      | Create
      | Join
      | Delete
    [@@deriving sexp_of]
  end

  type t = { room_id : Lobby.Room.Id.t; action : Room_action.t }
    [@@deriving sexp_of]
end

let room_row contents = Node.li [] contents

let player_row (p : Lobby.User.Player.t) ~all_scores =
  room_row
    [ Style.User.gen ~is_me:false p
    ; User_info.score_display ~all_scores p.role.score
    ]

let no_player_row =
  let nbsp = "\xc2\xa0" in
  room_row [Node.text nbsp]

let player_list ~(players : Lobby.User.Player.t Username.Map.t) =
  let num_players = Map.length players in
  let players = Map.data players in
  let all_scores = List.map players ~f:(fun p -> p.role.score) in
  [ List.map players ~f:(fun p -> player_row p ~all_scores)
  ; List.init (Int.max 0 (Params.num_players - num_players))
      ~f:(fun _ -> no_player_row)
  ] |> List.concat
  |> Node.ul [Attr.class_ "names"]

let mk_room ~header ~player_rows ~observer_spans ~footer =
  Node.div
    [ Attr.class_ "room" ]
    [ header
    ; player_rows
    ; Node.div [Attr.class_ "observers"]
        (Icon.observer :: observer_spans)
    ; footer
    ]

let view (model : Lobby.t) ~my_name ~(inject : Action.t -> _) =
  let view_room ~id ~(room : Lobby.Room.t) =
    let action act = inject { action = act; room_id = id } in
    let users = Lobby.Room.users room in
    let players, observers =
      Map.partition_map (Lobby.Room.users room)
        ~f:(fun user ->
            match Lobby.User.role user with
            | Observer o ->
                `Snd { user with role = o }
            | Player p ->
                `Fst { user with role = p }
          )
    in
    let delete_button =
      Node.button
        [ Attr.class_ "delete"
        ; Attr.on_click (fun _mouseEvent -> action Delete)
        ]
        [ Icon.delete ]
    in
    let id_item =
      Node.div
        [Attr.class_ "roomName"]
        (Node.text (Lobby.Room.Id.to_string id)
          :: if Lobby.Room.can_delete room then [delete_button] else []
        )
    in
    let join_button ~rejoin =
      let button_text = if rejoin then "rejoin" else "join" in
      Node.button
        [ Attr.class_ "room"
        ; Attr.on_click (fun _mouseEvent -> action Join)
        ]
        [Node.text button_text]
    in
    let observers =
      let shortener = Username.Shortener.of_list (Map.keys users) in
      List.concat_map (Map.data observers)
        ~f:(fun o ->
            let style =
              Style.Name.style
                ~is_me:(Username.equal o.username my_name)
                o.username
            in
            let attrs =
              List.filter_opt
                [ Option.some_if (not o.is_connected)
                    (Attr.class_ "disconnected")
                ; Option.some_if o.role.is_omniscient
                    (Attr.class_ "omniscient")
                ]
              @ [ Attr.class_ "name"
                ; Attr.style style
                ; Attr.create "title" (Username.to_string o.username)
                ]
            in
            let u = Username.Shortener.short shortener o.username in
            [ Node.text " "
            ; Node.span attrs [Node.text (Username.to_string u)]
            ]
          )
    in
    mk_room
      ~header:id_item
      ~player_rows:(player_list ~players)
      ~observer_spans:observers
      ~footer:(join_button ~rejoin:(Map.mem users my_name))
  in
  let create_room =
    mk_room
      ~header:(
        Widget.textbox
          ~id:Id.create_room
          ~classes:["roomName"]
          ~placeholder:"name"
          ~on_submit:(fun s ->
              let room_id = Lobby.Room.Id.of_string s in
              inject { action = Create; room_id }
            )
          ()
      )
      ~player_rows:(player_list ~players:Username.Map.empty)
      ~observer_spans:[]
      ~footer:(
        Node.button
          [ Attr.class_ "room"
          ; Attr.on_click (fun _mouseEvent ->
                let input =
                  Option.bind (Id.lookup_elt Id.create_room) ~f:(fun elt ->
                      Js.Opt.to_option (Dom_html.CoerceTo.input elt)
                    )
                in
                match input with
                | None -> Event.Ignore
                | Some input ->
                  let room_id =
                    input##.value
                    |> Js.to_string
                    |> Lobby.Room.Id.of_string
                  in
                  input##.value := Js.string "";
                  inject { action = Create; room_id }
              )
          ]
          [Node.text "create"]
      )
  in
  Map.to_alist model.rooms
  |> List.map ~f:(fun (id, room) -> view_room ~id ~room)
  |> fun rooms -> Node.div [Id.attr Id.rooms] (rooms @ [create_room])
