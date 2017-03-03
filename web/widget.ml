open Core_kernel.Std
open Incr_dom
open Vdom

let target_as_input keyboardEvent =
  Js.Opt.bind keyboardEvent##.target (fun target ->
    Dom_html.CoerceTo.input target)
  |> Js.Opt.to_option

let textbox
  ?id ?classes ?placeholder ?initial_value ?disabled ?(clear_on_submit=true)
  ?(on_keypress=fun ~self:_ _ -> Event.Ignore) ~on_submit ()
  =
  let handle_return keyboardEvent =
    match
      let open Option.Monad_infix in
      Option.some_if (keyboardEvent##.keyCode = 13) ()
      >>= fun () ->
      target_as_input keyboardEvent
      >>| fun input ->
      let ev = on_submit (Js.to_string input##.value) in
      begin if clear_on_submit
      then input##.value := Js.string ""
      end;
      ev
    with
    | Some ev -> ev
    | None -> Event.Ignore
  in
  let on_keypress keyboardEvent =
    let self = Option.value_exn (target_as_input keyboardEvent) in
    match on_keypress ~self keyboardEvent with
    | Event.Stop_propagation -> Event.Stop_propagation
    | ev -> Event.Many [ev; handle_return keyboardEvent]
  in
  let attrs =
    let add mkattr opt attrs =
      match opt with
      | None -> attrs
      | Some a -> mkattr a :: attrs
    in
    let disabled =
      Option.map disabled ~f:(fun b -> Js.Unsafe.inject (Js.bool b))
    in
    []
    |> add Attr.type_ (Some "text")
    |> add Attr.on_keypress (Some on_keypress)
    |> add Attr.id id
    |> add Attr.placeholder placeholder
    |> add Attr.value initial_value
    |> add (Attr.property "disabled") disabled
    |> add Attr.classes classes
  in
  Node.input attrs []
