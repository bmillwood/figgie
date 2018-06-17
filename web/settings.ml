open Core_kernel.Std
open Incr_dom
open Vdom

module Model = struct
  type t =
    { auto_cancel   : Auto_cancel.t
    ; enable_sounds : bool
    ; show_settings : bool
    } [@@deriving fields, sexp_of]

  let initial =
    { auto_cancel = Auto_cancel.initial
    ; enable_sounds = false
    ; show_settings = false
    }
end

module Action = struct
  type t =
    | Set of Model.t
    [@@deriving sexp_of]
end

let apply_action (Action.Set model) (_model : Model.t) =
  model

let view (model : Model.t) ~(inject : Action.t -> Event.t) =
  let settings =
    if model.show_settings
    then begin
      [ Node.text "sounds "
      ; Node.button
          [ Attr.on_click (fun _mouseEvent ->
                let enable_sounds = not model.enable_sounds in
                inject (Set { model with enable_sounds })
              )
          ]
          [ begin match model.enable_sounds with
            | true -> "enabled;"
            | false -> "disabled;"
            end |> Node.text
          ]
      ; Node.text " auto-cancel "
      ; Node.button
          [ Attr.on_click (fun _mouseEvent ->
                let auto_cancel : Auto_cancel.t =
                  match model.auto_cancel with
                  | Never      -> My_trades
                  | My_trades  -> Any_trades
                  | Any_trades -> Never
                in
                inject (Set { model with auto_cancel })
              )
          ]
          [ begin match model.auto_cancel with
            | Never -> "never."
            | My_trades -> "when I trade."
            | Any_trades -> "on any trade."
            end |> Node.text
          ]
      ; Node.text " "
      ; Node.button
          [ Attr.on_click (fun _mouseEvent ->
              inject (Set { model with show_settings = false })
            )
          ]
          [ Node.text "ok!" ]
      ]
    end else begin
      [ Node.button
          [ Attr.on_click (fun _mouseEvent ->
              inject (Set { model with show_settings = true })
            )
          ]
          [ Node.text "settings!" ]
      ]
    end
  in
  Node.div [ Id.attr Id.settings ] settings
