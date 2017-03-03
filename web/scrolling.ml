open Core_kernel.Std
open Incr_dom
open Vdom

module Model = struct
  type t =
    { id : string
    ; scroll_top : int
    ; scroll_height : int
    ; client_height : int
    } [@@deriving compare, sexp_of]

  let equal t1 t2 = compare t1 t2 = 0

  let of_elt elt =
    { id = Js.to_string elt##.id
    ; scroll_top = elt##.scrollTop
    ; scroll_height = elt##.scrollHeight
    ; client_height = elt##.clientHeight
    }

  let create ~id =
    { id; scroll_top = 0; scroll_height = 0; client_height = 0 }

  let scrolled_to_bottom t =
    t.scroll_height - t.scroll_top = t.client_height
end

module Action = struct
  type t = Scrolled
    [@@deriving sexp_of]
end

let apply_action (t : Model.t) Action.Scrolled =
  match Dom_html.getElementById t.id with
  | exception _ -> t
  | elt -> Model.of_elt elt

let scroll_to_bottom elt =
  elt##.scrollTop := elt##.scrollHeight - elt##.clientHeight

let on_scroll (_ : Model.t) ~(inject : Action.t -> _) =
  Attr.on "scroll" (fun _ev -> inject Scrolled)

let on_display (latest_snapshot : Model.t) ~schedule =
  let id = latest_snapshot.id in
  let elt = Dom_html.getElementById id in
  let current = Model.of_elt elt in
  if current.scroll_height > latest_snapshot.scroll_height
    && Model.scrolled_to_bottom latest_snapshot
  then (
    scroll_to_bottom (Dom_html.getElementById id)
  );
  if not (Model.equal latest_snapshot current) then (
    schedule Action.Scrolled
  )
