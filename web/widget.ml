open Core_kernel.Std
open Incr_dom.Std

let textbox ?id ?placeholder ?initial_value ?(clear_on_submit=true) ~f attrs =
  let handle_keypress keyboardEvent =
    match
      let open Option.Monad_infix in
      Option.some_if (keyboardEvent##.keyCode = 13) ()
      >>= fun () ->
      Js.Opt.to_option keyboardEvent##.target
      >>= fun target ->
      Js.Opt.to_option (Dom_html.CoerceTo.input target)
      >>| fun input ->
      let ev = f (Js.to_string input##.value) in
      begin if clear_on_submit
      then input##.value := Js.string ""
      end;
      ev
    with
    | Some ev -> ev
    | None -> Vdom.Event.Ignore
  in
  let attrs =
    let add mkattr opt attrs =
      match opt with
      | None -> attrs
      | Some a -> mkattr a :: attrs
    in
    attrs
    |> add Vdom.Attr.type_ (Some "text")
    |> add Vdom.Attr.on_keypress (Some handle_keypress)
    |> add Vdom.Attr.id id
    |> add Vdom.Attr.placeholder placeholder
    |> add Vdom.Attr.value initial_value
  in
  Vdom.Node.input attrs []
