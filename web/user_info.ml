open Core_kernel.Std
open Incr_dom
open Vdom

open Figgie
open Market

module Player = struct
  type t = Lobby.User.Player.t [@@deriving sexp]

  let nobody : t =
    { username = Username.of_string "[nobody]"
    ; is_connected = true
    ; role =
      { score = Price.zero
      ; hand = Partial_hand.empty
      ; is_ready = false
      }
    }

  let of_user user =
    match Lobby.User.role user with
    | Player p -> Some { user with role = p }
    | _ -> None
end

module Position = struct
  type t = | Me | Left | Middle | Right

  let class_ =
    function
    | Me -> "myself"
    | Left -> "left"
    | Middle -> "middle"
    | Right -> "right"
end

let score_display scores_map score =
  let ranking =
    let is_better_score s = Price.O.(s > score) in
    match Map.count scores_map ~f:is_better_score with
    | 0 -> "first"
    | 1 -> "second"
    | 2 -> "third"
    | _ -> "last"
  in
  Node.span
    [Attr.classes ["score"; ranking]]
    [Node.text (Price.to_string score)]

let container = Node.div [Id.attr Id.user_info]

let empty = container []

let hand ~gold (hand : Partial_hand.t) =
  let span_of_copies class_ n s =
    if Size.O.(n < zero) then (
      raise_s [%message "user appears to have negative cards"]
    );
    let content =
      List.init (Size.to_int n) ~f:(fun _ -> s)
      |> String.concat
      |> Node.text
    in
    Node.span [Attr.class_ class_] [content]
  in
  let known =
    Card.Hand.foldi hand.known
      ~init:[]
      ~f:(fun suit acc count ->
          Style.suit_span ~count:(Size.to_int count) ~gold suit
          :: acc)
    |> List.rev
  in
  let unknown_utf8 = "\xe2\x96\x88" in
  let unknown =
    span_of_copies "Unknown" hand.unknown unknown_utf8
  in
  known @ [unknown]

let player ~pos ~icons ~all_scores ~gold (player : Player.t) =
  let is_me =
    match pos with
    | Position.Me -> true
    | _ -> false
  in
  let name =
    let classes =
      "name" :: if player.is_connected then [] else ["disconnected"]
    in
    Hash_colour.username_span ~attrs:[Attr.classes classes] ~is_me
      player.username
  in
  Node.div [Attr.classes ["userinfo"; Position.class_ pos]] (
    [ [ name
      ; score_display all_scores player.role.score
      ]
    ; icons
    ; [ Node.create "br" [] [] ]
    ; hand ~gold player.role.hand
    ] |> List.concat
  )

let players ~others ~gold ~me =
  let (my_pers : Player.t), _ = me in
  let players = Map.add others ~key:my_pers.username ~data:me in
  let all_scores = Map.map players ~f:(fun (p, _) -> p.role.score) in
  let nobody = Player.nobody, [] in
  match Map.data others @ List.init 3 ~f:(fun _ -> nobody) with
  | left :: middle :: right :: _ ->
    let p ~pos (p, icons) =
      player ~pos ~icons ~all_scores ~gold p
    in
    container
      [ Node.div [Id.attr Id.others]
        [ p ~pos:Left   left
        ; p ~pos:Middle middle
        ; p ~pos:Right  right
        ]
      ; p ~pos:Me me
      ]
  | _ -> assert false

let others_and_me ~users ~my_name =
  let players = Map.filter_map users ~f:Player.of_user in
  let me =
    match Map.find players my_name with
    | None -> Player.nobody
    | Some me -> me
  in
  Map.remove players my_name, me

let waiting ~inject_I'm_ready ~users ~my_name ~last_gold =
  let others, me = others_and_me ~users ~my_name in
  let ready = "\xe2\x9c\x93" in
  let not_ready = "\xf0\x9f\x9a\xab" in
  let others =
    Map.map others ~f:(fun o ->
      let ready_text =
        if o.role.is_ready
        then ready
        else not_ready
      in
      (o, [Node.text ready_text]))
  in
  let ready_button =
    let text, set_it_to =
      if me.role.is_ready
      then not_ready, false
      else ready, true
    in
    Node.button
      [ Id.attr Id.ready_button
      ; Attr.on_click (fun _mouseEvent -> inject_I'm_ready set_it_to)
      ]
      [ Node.text text ]
  in
  players
    ~others
    ~gold:last_gold
    ~me:(me, [ready_button])

let playing ~(users : Lobby.User.t Username.Map.t) ~my_name =
  let others, me = others_and_me ~users ~my_name in
  players
    ~others:(Map.map others ~f:(fun p -> (p, [])))
    ~gold:None
    ~me:(me, [])
