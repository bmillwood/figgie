open Core_kernel.Std
open Incr_dom
open Vdom

open Figgie
open Market

module Player = struct
  type t = Lobby.User.Player.t

  let nobody : t =
    { username = Username.of_string "[nobody]"
    ; is_connected = true
    ; role = { score = Price.zero; hand = Partial_hand.empty }
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

let container = Node.div [Attr.id "userinfo"]

let empty = container []

let player ~pos ~(pers : Player.t) ~scores ~info =
  let is_me =
    match pos with
    | Position.Me -> true
    | _ -> false
  in
  let name =
    let classes =
      "name" :: if pers.is_connected then [] else ["disconnected"]
    in
    Hash_colour.username_span ~attrs:[Attr.classes classes] ~is_me
      pers.username
  in
  Node.div [Attr.classes ["userinfo"; Position.class_ pos]] (
    [ [ name
      ; score_display scores pers.role.score
      ; Node.create "br" [] []
      ]
    ; info
    ] |> List.concat
  )

let players ~others ~me =
  let (my_pers : Player.t), _ = me in
  let players = Map.add others ~key:my_pers.username ~data:me in
  let scores = Map.map players ~f:(fun (p, _) -> p.role.score) in
  let nobody = Player.nobody, [] in
  match Map.data others @ List.init 3 ~f:(fun _ -> nobody) with
  | left :: middle :: right :: _ ->
    let p ~pos ((pers : Player.t), info) =
      player ~pos ~pers ~scores ~info
    in
    container
      [ Node.div [Attr.id "others"]
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

let waiting
    ~inject_I'm_ready ~users ~my_name ~last_gold:_
    ~(who_is_ready : Username.Set.t)
  =
  let others, me = others_and_me ~users ~my_name in
  let others =
    Map.map others ~f:(fun o ->
      let ready_text =
        if Set.mem who_is_ready o.username
        then "[ready]"
        else "[not ready]"
      in
      (o, [Node.text ready_text]))
  in
  let ready_button =
    let is_ready = Set.mem who_is_ready me.username in
    let text, set_it_to =
      if is_ready
      then "I'm not ready!", false
      else "I'm ready!", true
    in
    Node.button
      [ Attr.id Ids.ready_button
      ; Attr.on_click (fun _mouseEvent -> inject_I'm_ready set_it_to)
      ]
      [ Node.text text ]
  in
  players
    ~others
    ~me:(me, [ready_button])

let playing ~(users : Lobby.User.t Username.Map.t) ~my_name =
  let others, me = others_and_me ~users ~my_name in
  let span_of_copies class_ n s =
    let content =
      List.init (Size.to_int n) ~f:(fun _ -> s)
      |> String.concat
      |> Node.text
    in
    Node.span [Attr.class_ class_] [content]
  in
  let pers_with_hand (player : Player.t) =
    let known =
      Card.Hand.foldi player.role.hand.known
        ~init:[]
        ~f:(fun suit acc count ->
          span_of_copies
            (Card.Suit.name suit)
            count
            (Card.Suit.to_utf8 suit)
            :: acc)
        |> List.rev
    in
    let unknown_utf8 = "\xe2\x96\x88" in
    let unknown =
      span_of_copies "Unknown" player.role.hand.unknown unknown_utf8
    in
    (player, known @ [unknown])
  in
  players
    ~others:(Map.map others ~f:pers_with_hand)
    ~me:(pers_with_hand me)
