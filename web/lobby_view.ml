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
    let num_players = Map.length users in
    let user_score (user : Lobby.User.t) =
      match user.role with
      | Player p -> Some p.score
      | Observer _ -> None
    in
    let scores = Map.filter_map users ~f:user_score in
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
      [ List.map (Map.to_alist users) ~f:(fun (username, user) ->
          Node.tr []
            (List.filter_opt
              [ if Username.equal username my_name then (
                  Some (Node.td [] [join_button ~rejoin:true])
                ) else (
                  let classes =
                    "name"
                    :: if user.is_connected then [] else ["disconnected"]
                  in
                  Some (
                    Node.td
                      [Attr.classes classes]
                      [Hash_colour.username_span ~is_me:false username]
                  )
                )
              ; Option.map (user_score user) ~f:(fun score ->
                    Node.td
                      [Attr.class_ "score"]
                      [User_info.score_display scores score]
                  )
              ]
            )
        )
      ; List.init (Int.max 0 (Lobby.room_size - num_players)) ~f:(fun i ->
          if i = 0 && not (Map.mem users my_name) then (
            Node.tr [] [Node.td [] [join_button ~rejoin:false]]
          ) else (
            let nbsp = "\xc2\xa0" in
            Node.tr [] [Node.td [Attr.class_ "name"] [Node.text nbsp]]
          )
        )
      ] |> List.concat
    in
    Node.div
      [ Attr.class_ "room" ]
      [ id_item
      ; Node.table [Attr.class_ "room"] players
      ]
  in
  Map.to_alist model.rooms
  |> List.map ~f:(fun (id, room) -> view_room ~id ~room)
  |> Node.div [Attr.id "rooms"]
