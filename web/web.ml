open Async_kernel.Std
open Core_kernel.Std
open Incr_dom.Std

module Figgie_web = struct
  module Connection_status = struct
    type t =
      | Connecting
      | Failed_to_connect
      | Connected
      | Disconnected
      [@@deriving sexp]
  end

  module Model = struct
    type t = {
      connection_status : Connection_status.t;
      broadcasts        : Protocol.Broadcast.t Fqueue.t;
      market            : Market.Book.t;
    }

    let initial =
      { connection_status = Connecting
      ; broadcasts = Fqueue.empty
      ; market = Market.Book.empty
      }

    let max_broadcasts = 10
  end

  module Action = struct
    type t =
      | Set_connection_status of Connection_status.t
      | Set_market of Market.Book.t
      | Add_broadcast of Protocol.Broadcast.t
      [@@deriving sexp]

    let apply action ~schedule:_ (model : Model.t) =
      match action with
      | Set_connection_status connection_status ->
        { model with connection_status }
      | Set_market market ->
        { model with market }
      | Add_broadcast broadcast ->
        let old_broadcasts =
          if Fqueue.length model.broadcasts = Model.max_broadcasts
          then Fqueue.discard_exn model.broadcasts
          else model.broadcasts
        in
        { model with broadcasts = Fqueue.enqueue old_broadcasts broadcast }

    let should_log _ = true
  end

  let status_span (connection_status : Connection_status.t) =
    let node ?(fg="black") ?(bg="transparent") text =
      Vdom.Node.span [Vdom.Attr.style ["color", fg; "background-color", bg]]
        [Vdom.Node.text text]
    in
    match connection_status with
    | Connecting        -> node ~bg:"yellow" "Connecting"
    | Failed_to_connect -> node ~fg:"white" ~bg:"red" "Failed"
    | Connected         -> node ~fg:"white" ~bg:"green" "Connected"
    | Disconnected      -> node ~fg:"white" ~bg:"red" "Disconnected"

  let string_of_broadcast (bc : Protocol.Broadcast.t) =
    match bc with
    | Player_joined u ->      sprintf !"%{Username} joined" u
    | Chat (u, m) ->          sprintf !"<%{Username}> %S" u m
    | Waiting_for n ->        sprintf "Need %d more players" n
    | Exec (_order, _exec) -> "EXEC"
    | Out _order ->           "OUT"
    | Round_over _results  -> "Round over!"

  let broadcasts_list broadcasts =
    List.map (Fqueue.to_list broadcasts) ~f:(fun broadcast ->
      Vdom.Node.li [] [Vdom.Node.text (string_of_broadcast broadcast)])
    |> Vdom.Node.ul []

  let view (incr_model : Model.t Incr.t) ~schedule:_ =
    let open Incr.Let_syntax in
    let%map { connection_status; broadcasts; market = _ } = incr_model in
    Vdom.Node.body []
      [ Vdom.Node.p  [] [status_span connection_status]
      ; broadcasts_list broadcasts
      ]
end

let read_updates_from ~host ~port =
  Deferred.create (fun read_from ->
    let socket =
      new%js WebSockets.webSocket
        (Js.string (sprintf "ws://%s:%d" host port))
    in
    let reader, writer = Pipe.create () in
    socket##.onopen := Dom.handler (fun _event ->
      Ivar.fill_if_empty read_from (Some reader);
      socket##.onmessage := Dom.handler (fun event ->
        let data = Js.to_string event##.data in
        Pipe.write_without_pushback writer
          ([%of_sexp: Protocol.Web_update.t] (Sexp.of_string data));
        Js._false
      );
      socket##.onclose := Dom.handler (fun _event ->
        Pipe.close writer;
        Js._false
      );
      Js._false
    );
    socket##.onerror := Dom.handler (fun _event ->
      Ivar.fill_if_empty read_from None;
      Js._false
    )
  )

let on_startup ~schedule _ =
  don't_wait_for begin
    let open Figgie_web.Action in
    read_updates_from ~host:"localhost" ~port:20406
    >>= function
    | None ->
      schedule (Set_connection_status Failed_to_connect);
      Deferred.unit
    | Some updates ->
      schedule (Set_connection_status Connected);
      Pipe.iter_without_pushback updates ~f:(function
        | Broadcast broadcast -> schedule (Add_broadcast broadcast)
        | Market market -> schedule (Set_market market))
      >>= fun () ->
      schedule (Set_connection_status Disconnected);
      Deferred.unit
  end

let () =
  Start_app.start
    ~initial_state:Figgie_web.Model.initial
    ~on_startup
    ~project_immutable_summary:Fn.id
    ~on_display:(fun ~schedule:_ ~old:_ _new -> ())
    (module Figgie_web)
