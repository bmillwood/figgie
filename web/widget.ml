open Core_kernel
open Incr_dom
open Vdom

let target_as_input_exn keyboardEvent =
  Js.Opt.bind keyboardEvent##.target (fun target ->
    Dom_html.CoerceTo.input target)
  |> Js.Opt.to_option
  |> Option.value_exn

let textbox
  ?id ?classes ?placeholder ?initial_value ?disabled ?(clear_on_submit=true)
  ?(on_keypress=fun ~self:_ _ -> Event.Ignore)
  ?(on_input=fun ~self:_ _ -> Event.Ignore)
  ~on_submit
  ()
  =
  let handle_return keyboardEvent =
    match
      let open Option.Monad_infix in
      Option.some_if (keyboardEvent##.keyCode = 13) ()
      >>| fun () ->
      let input = target_as_input_exn keyboardEvent in
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
    let self = target_as_input_exn keyboardEvent in
    match on_keypress ~self keyboardEvent with
    | Event.Stop_propagation -> Event.Stop_propagation
    | ev -> Event.Many [ev; handle_return keyboardEvent]
  in
  let wrapped_on_input event input =
    let self = target_as_input_exn event in
    on_input ~self input
  in
  let on_load event =
    let self = target_as_input_exn event in
    Dom_html.window##alert (Js.string "!!");
    on_input ~self ""
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
    |> add Attr.on_input (Some wrapped_on_input)
    |> add (Attr.on "load") (Some on_load)
    |> add Id.attr id
    |> add Attr.placeholder placeholder
    |> add Attr.value initial_value
    |> add (Attr.property "disabled") disabled
    |> add Attr.classes classes
  in
  Node.input attrs []
