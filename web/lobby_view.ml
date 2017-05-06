open Core_kernel.Std
open Incr_dom
open Vdom

open Figgie

module Action = struct
  type t =
    | Join_room of Lobby.Room.Id.t
    | Delete_room of Lobby.Room.Id.t
end

let view (model : Lobby.t) ~my_name ~(inject : Action.t -> _) =
  let view_room ~id ~(room : Lobby.Room.t) =
    let users = Lobby.Room.users room in
    let players, observers =
      Map.partition_map (Lobby.Room.users room)
        ~f:(fun user ->
            let username     = Lobby.User.username     user in
            let is_connected = Lobby.User.is_connected user in
            match Lobby.User.role user with
            | Observer { is_omniscient } ->
              `Snd (username, is_connected, is_omniscient)
            | Player { hand = _; score } ->
              `Fst (username, is_connected, score)
          )
    in
    let num_players = Map.length players in
    let scores = Map.map players ~f:(fun (_, _, score) -> score) in
    let delete_button =
      Node.button
        [ Attr.class_ "delete"
        ; Attr.on_click (fun _mouseEvent -> inject (Delete_room id))
        ]
        [ Node.text "\xc3\x97" ]
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
        [ Attr.class_ "join"
        ; Attr.on_click (fun _mouseEvent -> inject (Join_room id))
        ]
        [Node.text button_text]
    in
    let players =
      [ List.map (Map.data players)
          ~f:(fun (username, is_connected, score) ->
              Node.tr []
                [ if Username.equal username my_name then (
                      Node.td [] [join_button ~rejoin:true]
                    ) else (
                    let classes =
                      "name"
                      :: if is_connected then [] else ["disconnected"]
                    in
                    Node.td
                      [Attr.classes classes]
                      [Hash_colour.username_span ~is_me:false username]
                  )
                ; Node.td
                    [Attr.class_ "score"]
                    [User_info.score_display scores score]
                ]
            )
      ; List.init (Int.max 0 (Lobby.max_players_per_room - num_players))
          ~f:(fun i ->
            if i = 0 && not (Map.mem users my_name) then (
              Node.tr [] [Node.td [] [join_button ~rejoin:false]]
            ) else (
              let nbsp = "\xc2\xa0" in
              Node.tr [] [Node.td [Attr.class_ "name"] [Node.text nbsp]]
            )
          )
      ] |> List.concat
    in
    let observers =
      let keep_trues =
        List.filter_map ~f:(fun (cond, x) -> Option.some_if cond x)
      in
      let shortener = Username.Shortener.of_list (Map.keys users) in
      List.concat_mapi (Map.data observers)
        ~f:(fun i (username, is_connected, is_omniscient) ->
            let style =
              Hash_colour.username_style
                ~is_me:(Username.equal username my_name)
                username
            in
            let attrs =
              keep_trues
                [ not is_connected, Attr.class_ "disconnected"
                ; is_omniscient,    Attr.class_ "omniscient"
                ; true,             Attr.class_ "name"
                ; true,             Attr.style style
                ]
            in
            let u = Username.Shortener.short shortener username in
            keep_trues
              [ i = 0, Node.text "\xf0\x9f\x91\x81"
              ; true, Node.text " "
              ; true, Node.span attrs [Node.text (Username.to_string u)]
              ]
          )
    in
    Node.div
      [ Attr.class_ "room" ]
      [ id_item
      ; Node.table [Attr.class_ "room"] players
      ; Node.div [Attr.class_ "observers"] observers
      ]
  in
  Map.to_alist model.rooms
  |> List.map ~f:(fun (id, room) -> view_room ~id ~room)
  |> Node.div [Attr.id "rooms"]
