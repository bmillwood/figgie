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
    let (!!) = clockwise_quarter_turns_from_north in
    [| Near; Left; Far; Right |].((!! seat - !! near) % 4)

  let to_seat ~south pos =
    let clockwise_quarter_turns_from_near =
      function | Near -> 0 | Left -> 1 | Far -> 2 | Right -> 3
    in
    let (!!) = clockwise_quarter_turns_from_near in
    [| Lobby.Room.Seat.South; West; North; East |].((!! pos - !! south) % 4)

  let class_ =
    function
    | Near -> "near"
    | Left -> "left"
    | Far -> "far"
    | Right -> "right"
end

module Action = struct
  type t =
    | Set_ready of bool
    | Sit of Position.t
    | Request_bot of
        { pos : Position.t
        ; type_ : Protocol.Bot_type.t
        }
  [@@deriving sexp_of]
end
open Action

let near_seat ~room ~my_name : Lobby.Room.Seat.t =
  let seat_and_user_list = Map.to_alist (Lobby.Room.seating room) in
  let is_me (_p, u) = Username.equal u my_name in
  match List.find seat_and_user_list ~f:is_me with
  | None -> South
  | Some (seat, _) -> seat

let apply_action action () ~conn ~room_id ~room ~my_name =
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
  | Sit pos ->
    let near_seat = near_seat ~room ~my_name in
    let south = Position.of_seat ~near:near_seat South in
    let seat = Position.to_seat ~south pos in
    don't_wait_for begin
      Rpc.Rpc.dispatch_exn Protocol.Start_playing.rpc conn (Sit_in seat)
      >>| function
      | Error (`Not_logged_in | `Not_in_a_room) -> assert false
      | Error (`Game_already_started | `Seat_occupied) -> ()
      | Error `You're_already_playing | Ok (_ : Lobby.Room.Seat.t) -> ()
    end
  | Request_bot { pos; type_ } ->
    let near_seat = near_seat ~room ~my_name in
    let south = Position.of_seat ~near:near_seat South in
    let seat = Position.to_seat ~south pos in
    don't_wait_for begin
      Rpc.Rpc.dispatch_exn Protocol.Request_bot.rpc conn
        { type_; room = room_id; seat }
      >>| function
      | Error `No_such_room -> assert false
      | Error `Seat_occupied -> ()
      | Ok () -> ()
    end

let name ~username ~is_me ~colour ~score =
  let attrs =
    [ Some (Attr.class_ "name")
    ; Option.some_if colour
        (Attr.style (Style.Name.style ~is_me username))
    ] |> List.filter_opt
  in
  Node.div attrs (
    [ Some (Node.text (Username.to_string username))
    ; score
    ] |> List.filter_opt
  )

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
  let near = near_seat ~room ~my_name in
  Map.fold (Lobby.Room.seating room) ~init:Position.Map.empty
    ~f:(fun ~key:seat ~data:username acc ->
        Option.fold
          (Option.bind (Map.find (Lobby.Room.users room) username)
             ~f:Lobby.User.to_player)
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

let nobody ~inject ~pos ~can_sit =
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
      | Some type_ -> inject (Request_bot { pos; type_ })
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
            [Attr.on_click (fun _mouseEvent -> inject (Sit pos))]
            [Node.text "Join"]
        )
      ; (true, add_bot)
      ]
  in
  Node.div [Attr.classes ["userinfo"]]
    [ name
        ~username:(Username.of_string "[nobody]")
        ~is_me:false
        ~colour:false
        ~score:None
    ; Node.div [] buttons
    ]

let ready_button ~inject ~am_ready =
  let text, class_, set_it_to =
    if am_ready
    then "cancel ready", "notReady", false
    else "click when ready", "ready", true
  in
  Node.button
    [ Id.attr Id.ready_button
    ; Attr.class_ class_
    ; Attr.on_click (fun _mouseEv -> inject (Set_ready set_it_to))
    ]
    [ Node.text text ]

let somebody ~inject ~is_me ~all_scores ~gold (player : Lobby.User.Player.t) =
  let ready_class, button =
    match player.role.phase with
    | Playing -> (None, None)
    | Waiting { is_ready } ->
      ( Some (if is_ready then "ready" else "notReady")
      , if is_me
        then Some (ready_button ~inject ~am_ready:is_ready)
        else None
      )
  in
  let classes =
    List.filter_opt
      [ Some "userinfo"
      ; Option.some_if is_me "me"
      ; ready_class
      ]
  in
  Node.div [Attr.classes classes] (
    [ [ name
          ~username:player.username
          ~is_me
          ~colour:true
          ~score:(Some (score_display ~all_scores player.role.score))
      ; Node.div [] (hand ~gold player.role.hand)
      ]
    ; Option.to_list button
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
  let observer_spans =
    List.concat_map observers ~f:(fun o ->
        let is_me = Username.equal o.username my_name in
        [ Node.text " "
        ; Style.User.Observer.span ~is_me o
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
          let can_sit =
            Option.exists (Map.find (Lobby.Room.users room) my_name)
              ~f:(fun me ->
                  match me.role with
                  | Observer _ -> true
                  | Player _ -> false
                )
          in
          nobody ~inject ~pos ~can_sit
        | Some player ->
          let is_me = Username.equal my_name player.username in
          let player =
            if is_me
            then { player with role = { player.role with hand = my_hand } }
            else player
          in
          somebody ~inject ~is_me ~all_scores ~gold player
      ]
  in
  Node.div [Id.attr Id.user_info]
    [ in_position Far
    ; Node.div [Attr.class_ "mid"] (
        [ Some (in_position Left)
        ; Some (in_position Right)
        ] |> List.filter_opt
      )
    ; in_position Near
    ; observer_row ~my_name ~room
    ]
