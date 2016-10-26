open Core_kernel.Std

let focus_input ~id =
  let (>>>) t f = Option.iter t ~f in
  Option.try_with (fun () -> Dom_html.getElementById id)
  >>> fun elt ->
  Js.Opt.to_option (Dom_html.CoerceTo.input elt)
  >>> fun input ->
  input##focus

let get () =
  Option.map
    (Js.Opt.to_option Dom_html.document##.activeElement)
    ~f:(fun elt -> Js.to_string elt##.id)
