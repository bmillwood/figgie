open Core_kernel.Std
open Incr_dom
open Vdom

open Market

module Position = struct
  type t = | Me | Left | Middle | Right

  let attr t =
    let class_ =
      match t with
      | Me -> "myself"
      | Left -> "left"
      | Middle -> "middle"
      | Right -> "right"
    in
    Attr.class_ class_
end

let infobox ~pos ~name ~score ~info =
  Node.div [Position.attr pos]
    ([ [name]
    ; Option.to_list score
    ; [Node.create "br" [] []]
    ; info
    ] |> List.concat)

let container = Node.div [Attr.id "infoboxes"]

let empty = container []

let player ~pos ~(pers : Player.Persistent.t) ~ranking ~info =
  let score =
    Node.span
      [Attr.classes ["score"; ranking]]
      [Node.text (Price.to_string pers.score)]
  in
  let is_me =
    match pos with
    | Position.Me -> true
    | _ -> false
  in
  let name =
    Hash_colour.username_span ~attrs:[Attr.class_ "name"] ~is_me
      pers.username
  in
  infobox ~pos ~name ~score:(Some score) ~info

let players ~others ~me =
  let (my_pers : Player.Persistent.t), _ = me in
  let players = Map.add others ~key:my_pers.username ~data:me in
  let nobody = Player.Persistent.nobody, [] in
  match Map.data others @ List.init 3 ~f:(fun _ -> nobody) with
  | left :: middle :: right :: _ ->
    let p ~pos ((pers : Player.Persistent.t), info) =
      let better_players =
        Map.count players ~f:(fun (o, _) ->
          Price.O.(o.score > pers.score))
      in
      let ranking =
        match better_players with
        | 0 -> "first"
        | 1 -> "second"
        | 2 -> "third"
        | _ -> "last"
      in
      player ~pos ~pers ~ranking ~info
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

let waiting
    ~inject_I'm_ready
    ~(me : Player.Persistent.t)
    ~(others : Player.Persistent.t Username.Map.t)
    ~(who_is_ready : Username.Set.t)
  =
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

let playing
    ~(others : Player.t Username.Map.t)
    ~(me : Player.t)
    =
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
      Card.Hand.foldi player.hand.known
        ~init:[]
        ~f:(fun suit acc count ->
          span_of_copies
            (Card.Suit.name suit)
            count
            (Card.Suit.to_utf8 suit)
            :: acc)
        |> List.rev
    in
    let unknown =
      span_of_copies "Unknown" player.hand.unknown Partial_hand.unknown_utf8
    in
    (player.pers, known @ [unknown])
  in
  players
    ~others:(Map.map others ~f:pers_with_hand)
    ~me:(pers_with_hand me)
