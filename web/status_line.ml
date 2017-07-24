open Core_kernel.Std
open Incr_dom
open Vdom

open Figgie

module Model = struct
  type t =
    | Not_connected of
        { conn_error : Connection_error.t option
        ; input_error : bool
        ; connectbox_prefill : string option
        }
    | Connecting of Host_and_port.t
    | Connected of Host_and_port.t
    | Logged_in of
        { connected_to : Host_and_port.t
        ; username : Username.t
        ; room_id : Lobby.Room.Id.t option
        ; clock : Countdown.Model.t option
        }
end

let set_username_field_colour ~self input =
  (* this is so silly but I love it *)
  self##.style##.backgroundColor := Js.string (
    let u = Username.of_string input in
    if String.is_empty input || Username.is_valid u then (
      Style.Name.colour (Username.of_string input)
    ) else (
      "red"
    )
  )

module Action = struct
  type t =
    | Input_error
    | Input_ok
    | Start_connecting_to of Host_and_port.t
    | Log_in of Username.t
    [@@deriving sexp_of]
end

let view (model : Model.t) ~(inject : Action.t -> _) =
  let line ~class_ ~status =
    Node.p [Id.attr Id.status; Attr.class_ class_] status
  in
  match model with
  | Not_connected noconn ->
    let connectbox =
      let classes = Option.some_if noconn.input_error ["error"] in
      Widget.textbox ~id:Id.connect_to ?classes
        ?initial_value:noconn.connectbox_prefill
        ~placeholder:"host[:port]" ~clear_on_submit:false
        ~on_input:(fun ~self:_ hps ->
            inject (
              match Parse.host_and_port hps with
              | Some _ -> Input_ok
              | None   -> Input_error
            )
          )
        ~on_submit:(fun hps ->
            inject (
              match Parse.host_and_port hps with
              | Some hp -> Start_connecting_to hp
              | None    -> Input_error
            )
          )
        ()
    in
    let status =
      [ Node.text (
          match noconn.conn_error with
          | None -> "Connect to:"
          | Some Failed_to_connect -> "Connection failed. Retry:"
          | Some Connection_lost   -> "Connection lost. Reconnect:"
        )
      ; connectbox
      ]
    in
    let class_ =
      if Option.is_none noconn.conn_error
      then "Disconnected"
      else "ConnectionFailed"
    in
    line ~class_ ~status
  | Connecting hp ->
    line ~class_:"Connecting"
      ~status:[Node.text ("Connecting to " ^ Host_and_port.to_string hp)]
  | Connected hp ->
    line ~class_:"Connected"
      ~status:
        [ Widget.textbox ~id:Id.login
            ~placeholder:"username"
            ?initial_value:Url_vars.username
            ~on_submit:(fun user ->
                inject (Log_in (Username.of_string user))
              )
            ~clear_on_submit:false
            ~on_input:(fun ~self input ->
                set_username_field_colour ~self input;
                Event.Ignore
              )
            ()
        ; Node.text (" connected to " ^ Host_and_port.to_string hp)
        ]
  | Logged_in { connected_to; username; room_id; clock } ->
    line ~class_:"Connected"
      ~status:(
          [ Some (Style.Name.span ~is_me:true username)
          ; Some (Node.text (
              " connected to " ^ Host_and_port.to_string connected_to
            ))
          ; Option.map room_id ~f:(fun room_name -> Node.text (
              sprintf " in room %S" (Lobby.Room.Id.to_string room_name)
            ))
          ; Option.map clock ~f:(fun clock ->
              Node.span [Attr.class_ "clock"]
                [Node.text (Countdown.Model.to_string clock)]
            )
          ] |> List.filter_opt
        )

let on_display ~(old : Model.t) (new_ : Model.t) =
  match new_ with
  | Connected _ ->
    begin match old with
    | Not_connected _ | Connecting _ ->
      Focus.with_input ~id:Id.login ~f:(fun input ->
        let content = Js.to_string input##.value in
        set_username_field_colour ~self:input content;
        input##focus;
        let n = String.length content in
        input##.selectionStart := n;
        input##.selectionEnd := n)
    | Connected _ | Logged_in _ -> ()
    end
  | Logged_in _ ->
    begin match old with
    | Not_connected _ | Connecting _ | Connected _ ->
      (* would like to focus ready button here, but buttonElement doesn't
         seem to have a focus method *)
      ()
    | Logged_in _ -> ()
    end
  | Not_connected _ | Connecting _ -> ()
