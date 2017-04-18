open Core_kernel.Std
open Incr_dom

type t = (char * string) array

let lookup_char t key =
  Array.find_map t ~f:(fun (c, s) ->
    Option.some_if (Char.equal key c) s)

let lookup_id t key =
  Array.find_map t ~f:(fun (c, s) ->
    Option.some_if (String.equal key s) c)

let placeholder_of_id t id =
  Option.map (lookup_id t id) ~f:(fun c -> String.of_char (Char.uppercase c))

let suppress_hotkeys_if_focused =
  String.Set.of_list [Ids.login; Ids.cmdline; Ids.connectTo]

let char_code ev =
  Option.bind (Js.Optdef.to_option ev##.charCode) ~f:Char.of_int

let on_keypress (t : t) (ev : Dom_html.keyboardEvent Js.t) =
  match Option.bind (char_code ev) ~f:(lookup_char t) with
  | None -> Vdom.Event.Ignore
  | Some id ->
    let suppress_because_focus =
      match Focus.get () with
      | None -> false
      | Some active_id ->
        Set.mem suppress_hotkeys_if_focused active_id
          || String.equal id active_id
    in
    let suppress_because_modifier =
      Js.to_bool ev##.altKey || Js.to_bool ev##.ctrlKey
    in
    if suppress_because_focus || suppress_because_modifier
    then Vdom.Event.Ignore
    else begin
      Focus.focus_input ~id;
      Vdom.Event.(Many [Stop_propagation; Prevent_default])
    end

module Global = struct
  let t : t =
    [| 'q', Ids.order ~dir:Sell ~suit:Spades
    ;  'w', Ids.order ~dir:Sell ~suit:Hearts
    ;  'e', Ids.order ~dir:Sell ~suit:Diamonds
    ;  'r', Ids.order ~dir:Sell ~suit:Clubs
    ;  'a', Ids.order ~dir:Buy  ~suit:Spades
    ;  's', Ids.order ~dir:Buy  ~suit:Hearts
    ;  'd', Ids.order ~dir:Buy  ~suit:Diamonds
    ;  'f', Ids.order ~dir:Buy  ~suit:Clubs
    |]

  let placeholder_of_id s = placeholder_of_id t s

  let lookup_id s = lookup_id t s

  let handler = Vdom.Attr.on_keypress (on_keypress t)
end
