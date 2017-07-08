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
    | Set_ready of bool
    | Sit of Lobby.Room.Seat.t
    | Request_bot of
        { seat : Lobby.Room.Seat.t
        ; type_ : Protocol.Bot_type.t
        }
  [@@deriving sexp_of]
end
open Action

let apply_action action () ~conn ~room_id =
  match action with
  | Set_ready readiness ->
    don't_wait_for begin
      Rpc.Rpc.dispatch_exn Protocol.Is_ready.rpc conn readiness
      >>| function
      | Ok ()
      | Error
          ( `Game_already_started
          | `You're_not_playing
          | `Not_logged_in
          | `Not_in_a_room
          ) -> ()
    end
  | Sit seat ->
    don't_wait_for begin
      Rpc.Rpc.dispatch_exn Protocol.Start_playing.rpc conn (Sit_in seat)
      >>| function
      | Error (`Not_logged_in | `Not_in_a_room) -> assert false
      | Error (`Game_already_started | `Seat_occupied) -> ()
      | Error `You're_already_playing | Ok (_ : Lobby.Room.Seat.t) -> ()
    end
  | Request_bot { seat; type_ } ->
    don't_wait_for begin
      Rpc.Rpc.dispatch_exn Protocol.Request_bot.rpc conn
        { type_; room = room_id; seat }
      >>| function
      | Error `No_such_room -> assert false
      | Error `Seat_occupied -> ()
      | Ok () -> ()
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

  let of_seat ~near seat =
    let clockwise_quarter_turns_from_north : Lobby.Room.Seat.t -> _ =
      function | North -> 0 | East -> 1 | South -> 2 | West -> 3
    in
    [| Near; Left; Far; Right |].(
      ( clockwise_quarter_turns_from_north seat
      - clockwise_quarter_turns_from_north near
      ) % 4
    )

  let to_seat ~south pos =
    let clockwise_quarter_turns_from_near =
      function | Near -> 0 | Left -> 1 | Far -> 2 | Right -> 3
    in
    [| Lobby.Room.Seat.South; West; North; East |].(
      ( clockwise_quarter_turns_from_near pos
      - clockwise_quarter_turns_from_near south
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
  let near : Lobby.Room.Seat.t =
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
                ~key:(Position.of_seat ~near seat)
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

let nobody ~inject ~seat ~can_sit =
  let add_bot =
    let no_bot = "" in
    let bots_by_value =
      String.Map.of_alist_exn
        (List.map Protocol.Bot_type.all ~f:(fun bot_type ->
            sprintf !"%{sexp:Protocol.Bot_type.t}" bot_type, bot_type
          ))
    in
    let handle_change _changeEvent new_value =
      match Map.find bots_by_value new_value with
      | None -> Event.Ignore
      | Some type_ -> inject (Request_bot { seat; type_ })
    in
    let options =
      Node.option [Attr.value no_bot] [Node.text "Bot"]
      :: List.map (Map.keys bots_by_value) ~f:(fun value ->
          Node.option [Attr.value value] [Node.text value]
        )
    in
    Node.select
      [ Attr.class_ "addBot"
      ; Attr.on_change handle_change
      ]
      options
  in
  let buttons =
    List.filter_map ~f:(fun (cond, elt) -> Option.some_if cond elt)
      [ ( can_sit
        , Node.button
            [Attr.on_click (fun _mouseEvent -> inject (Sit seat))]
            [Node.text "Join"]
        )
      ; (true, add_bot)
      ]
  in
  Node.div [Attr.classes ["userinfo"]]
    [ Node.span [Attr.class_ "name"] [Node.text "[nobody]"]
    ; Node.create "br" [] []
    ; Node.span [] buttons
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
            ; Attr.on_click (fun _mouseEvent -> inject (Set_ready set_it_to))
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

let observer_row ~my_name ~room =
  let observers =
    Map.data (Lobby.Room.users room)
    |> List.filter_map ~f:(fun user ->
        match Lobby.User.role user with
        | Player _ -> None
        | Observer o -> Some { user with role = o }
      )
  in
  let keep_trues =
    List.filter_map ~f:(fun (cond, x) -> Option.some_if cond x)
  in
  let observer_spans =
    List.concat_map observers ~f:(fun o ->
        let attrs =
          keep_trues
            [ not o.is_connected, Attr.class_ "disconnected"
            ; o.role.is_omniscient,    Attr.class_ "omniscient"
            ]
        in
        let is_me = Username.equal o.username my_name in
        [ Node.text " "
        ; Hash_colour.username_span ~attrs ~is_me o.username
        ]
      )
  in
  Node.div
    [ Attr.class_ "observers" ]
    (Icon.observer :: observer_spans)

let view () ~inject ~room ~my_hand ~my_name ~gold =
  let seating = people_in_places ~my_name ~room in
  let all_scores = List.map (Map.data seating) ~f:(fun p -> p.role.score) in
  let in_position pos =
    Node.div [Attr.class_ (Position.class_ pos)]
      [ match Map.find seating pos with
        | None ->
          let seat = Position.to_seat ~south:Near pos in
          let can_sit =
            Option.exists (Map.find (Lobby.Room.users room) my_name)
              ~f:(fun me ->
                  match me.role with
                  | Observer _ -> true
                  | Player _ -> false
                )
          in
          nobody ~inject ~seat ~can_sit
        | Some player ->
          let is_me = Username.equal my_name player.username in
          let player =
            if is_me
            then { player with role = { player.role with hand = my_hand } }
            else player
          in
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
    ; observer_row ~my_name ~room
    ]
