open Figgie

type t = settings:Settings.Model.t -> unit

let audio name =
  let elt = Dom_html.createAudio Dom_html.document in
  elt##.src := (Js.string ("sounds/" ^ name ^ ".wav"));
  fun ~settings ->
    if Settings.Model.enable_sounds settings
    then elt##play

let start = audio "start"
let tick = audio "tick"
let end_ = audio "end"

let trade ~(suit : Card.Suit.t) =
  audio (Card.Suit.name suit)
