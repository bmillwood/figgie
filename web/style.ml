open Core_kernel.Std
open Incr_dom
open Vdom

open Figgie

let suit_span ?(count=1) ~gold suit =
  let classes =
    Card.Suit.name suit
    :: (if Option.exists gold ~f:(Card.Suit.equal suit) then ["gold"] else [])
  in
  Node.span [Attr.classes classes]
    (List.init count ~f:(fun _ -> Icon.suit suit))

module Name = struct
  (* http://phrogz.net/css/distinct-colors.html *)
  let colours =
    [|
      "#ff7070"; "#ff2321"; "#cfa191"; "#fea27a"; "#ff5800"
    ; "#d0b494"; "#d78e2b"; "#f5c000"; "#999600"; "#b1bd9d"
    ; "#a4d483"; "#3da600"; "#00ed76"; "#00daa4"; "#00b5ae"
    ; "#00ddff"; "#00b6ff"; "#a4bccb"; "#5e77ff"; "#aeb0ff"
    ; "#ff7fff"; "#cc9cc7"; "#d5c1d0"; "#ff68a6"; "#ff0063"
    ; "#c49196"
    |]

  let colour u =
    colours.(Username.hash u mod Array.length colours)

  let style ~is_me u =
    let colour = colour u in
    if is_me
    then [("color", "black"); ("background-color", colour)]
    else [("color", colour)]

  let span' ?(extra_classes=[]) ~is_me u =
    Node.span
      [ Attr.style (style ~is_me u)
      ; Attr.classes ("name" :: extra_classes)
      ]
      [ Node.text (Username.to_string u) ]

  let span = span' ~extra_classes:[]
end

module User = struct
  module Gen = struct
    let classes (u : _ Lobby.User.Gen.t) =
      if u.is_connected then [] else ["disconnected"]

    let span ~is_me (u : _ Lobby.User.Gen.t) =
      Name.span' ~extra_classes:(classes u) ~is_me u.username
  end

  module Observer = struct
    let classes (o : Lobby.User.Observer.t) =
      (if o.role.is_omniscient then ["omniscient"] else [])
      @ Gen.classes o

    let span ~is_me (o : Lobby.User.Observer.t) =
      Name.span' ~extra_classes:(classes o) ~is_me o.username
  end
end
