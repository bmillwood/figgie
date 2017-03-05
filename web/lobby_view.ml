open Core_kernel.Std
open Incr_dom
open Vdom

module Action = struct
  type t =
    | Join_room of Lobby.Room.Id.t
end

let view (model : Lobby.t) ~my_name ~(inject : Action.t -> _) =
  let view_room ~id ~(room : Lobby.Room.t) =
    let nobody =
      let nbsp = "\xc2\xa0" in
      Node.li [Attr.class_ "name"] [Node.text nbsp]
    in
    let players = List.filter room.players ~f:(Username.(<>) my_name) in
    let num_players = List.length players in
    let id_item =
      Node.li [Attr.class_ "roomName"]
        [Node.text (Lobby.Room.Id.to_string id)]
    in
    let join_button =
      Node.li [Attr.class_ "name"]
        [ Node.button
            [ Attr.class_ "join"
            ; Attr.on_click (fun _mouseEvent -> inject (Join_room id))
            ]
            [Node.text "join"]
        ]
    in
    let players =
      [ List.map players ~f:(fun username ->
          let style =
            Hash_colour.username_style ~is_me:false username
          in
          Node.li [Attr.class_ "name"; Attr.style style]
            [Node.text (Username.to_string username)]
        )
      ; List.init (Int.max 0 (Lobby.room_size - num_players - 1))
          ~f:(fun _ -> nobody)
      ; if num_players < Lobby.room_size then [join_button] else []
      ] |> List.concat
    in
    Node.div [Attr.class_ "room"] [Node.ul [] (id_item :: players)]
  in
  Map.to_alist model.rooms
  |> List.map ~f:(fun (id, room) -> view_room ~id ~room)
  |> Node.div [Attr.id "rooms"]
