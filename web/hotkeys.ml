open Core_kernel.Std
open Incr_dom.Std

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

let on_keypress (t : t) (ev : Dom_html.keyboardEvent Js.t) =
  let open Option.Let_syntax in
  match
    let%bind charCode = Js.Optdef.to_option ev##.charCode in
    let%bind char = Char.of_int charCode in
    lookup_char t char
  with
  | None -> Vdom.Event.Ignore
  | Some id ->
    let suppress =
      match Focus.get () with
      | None -> false
      | Some active_id ->
        Set.mem suppress_hotkeys_if_focused active_id
          || String.equal id active_id
    in
    if suppress
    then Vdom.Event.Ignore
    else begin
      Focus.focus_input ~id;
      Vdom.Event.(Many [Stop_propagation; Prevent_default])
    end
