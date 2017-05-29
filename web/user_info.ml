open Core_kernel.Std
open Async_kernel
open Async_rpc_kernel
open Incr_dom
open Vdom

open Figgie
open Market

module Model = struct
  type t = unit
  let initial = ()
end

module Action = struct
  type t =
    | I'm_ready of bool
  [@@deriving sexp_of]
end
open Action

let apply_action (I'm_ready readiness) () ~conn =
  don't_wait_for begin
    Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc conn readiness
    >>| function
    | Ok ()
    | Error
        ( `Game_already_in_progress
        | `You're_not_playing
        | `Not_logged_in
        | `Not_in_a_room
        ) -> ()
  end

module Player = struct
  type t = Lobby.User.Player.t [@@deriving sexp]

  let of_user user =
    match Lobby.User.role user with
    | Player p -> Some { user with role = p }
    | _ -> None
end

module Position = struct
  module T = struct
    type t =
      | Near | Left | Far | Right
      [@@deriving compare, sexp]
  end
  include T
  include Comparable.Make(T)

  let of_seat ~from seat =
    let clockwise_quarter_turns_from_north : Lobby.Room.Seat.t -> _ =
      function | North -> 0 | East -> 1 | South -> 2 | West -> 3
    in
    [| Near; Left; Far; Right |].(
      ( clockwise_quarter_turns_from_north seat
      - clockwise_quarter_turns_from_north from
      ) % 4
    )

  let class_ =
    function
    | Near -> "near"
    | Left -> "left"
    | Far -> "far"
    | Right -> "right"
end

let score_display ~all_scores score =
  let ranking =
    match List.count all_scores ~f:(fun s -> Price.O.(s > score)) with
    | 0 -> "first"
    | 1 -> "second"
    | 2 -> "third"
    | _ -> "last"
  in
  Node.span
    [Attr.classes ["score"; ranking]]
    [Node.text (Price.to_string score)]

let people_in_places ~my_name ~room =
  let seating_by_name =
    Map.fold (Lobby.Room.seating room) ~init:Username.Map.empty
      ~f:(fun ~key:data ~data:key acc -> Map.add acc ~key ~data)
  in
  let from : Lobby.Room.Seat.t =
    match Map.find seating_by_name my_name with
    | None -> South
    | Some seat -> seat
  in
  Map.fold (Lobby.Room.seating room) ~init:Position.Map.empty
    ~f:(fun ~key:seat ~data:username acc ->
        Option.fold
          (Option.bind (Map.find (Lobby.Room.users room) username)
             ~f:Player.of_user)
          ~init:acc
          ~f:(fun acc player ->
              Map.add
                acc
                ~key:(Position.of_seat ~from seat)
                ~data:player
            )
      )

let hand ~gold (hand : Partial_hand.t) =
  let known =
    Card.Hand.foldi hand.known
      ~init:[]
      ~f:(fun suit acc count ->
          Style.suit_span ~count:(Size.to_int count) ~gold suit
          :: acc)
    |> List.rev
  in
  let unknown =
    Node.span [Attr.class_ "Unknown"]
      (List.init (Size.to_int hand.unknown) ~f:(fun _ -> Icon.unknown_suit))
  in
  known @ [unknown]

let nobody =
  Node.div [Attr.classes ["userinfo"]]
    [ Node.span [Attr.class_ "name"] [Node.text "[nobody]"]
    ; Node.create "br" [] []
    ; Node.text (let nbsp = "\xc2\xa0" in nbsp)
    ]

let somebody ~is_me ~all_scores ~gold ~inject (player : Player.t) =
  let name =
    let classes =
      "name" :: if player.is_connected then [] else ["disconnected"]
    in
    Hash_colour.username_span ~attrs:[Attr.classes classes] ~is_me
      player.username
  in
  let ready =
    match player.role.phase with
    | Playing -> []
    | Waiting { is_ready } ->
      if is_me then (
        let icon, set_it_to =
          if is_ready
          then Icon.not_ready, false
          else Icon.ready, true
        in
        [ Node.button
            [ Id.attr Id.ready_button
            ; Attr.on_click (fun _mouseEvent -> inject (I'm_ready set_it_to))
            ]
            [ icon ]
        ]
      ) else (
        if is_ready
        then [Icon.ready]
        else [Icon.not_ready]
      )
  in
  let classes =
    "userinfo"
    :: if is_me then ["me"] else []
  in
  Node.div [Attr.classes classes] (
    [ [name]
    ; ready
    ; [ score_display ~all_scores player.role.score
      ; Node.create "br" [] []
      ]
    ; hand ~gold player.role.hand
    ] |> List.concat
  )

let view () ~inject ~room ~my_name ~gold =
  let seating = people_in_places ~my_name ~room in
  let all_scores = List.map (Map.data seating) ~f:(fun p -> p.role.score) in
  let in_position pos =
    Node.div [Attr.class_ (Position.class_ pos)]
      [ match Map.find seating pos with
        | None -> nobody
        | Some player ->
          let is_me = Username.equal my_name player.username in
          somebody ~is_me ~all_scores ~gold ~inject player
      ]
  in
  Node.div [Id.attr Id.user_info]
    [ in_position Far
    ; Node.div [Attr.class_ "mid"]
        [ in_position Left
        ; in_position Right
        ]
    ; in_position Near
    ]
