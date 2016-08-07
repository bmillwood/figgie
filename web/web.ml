open Async_kernel.Std
open Core_kernel.Std

let read_from ~host ~port =
  Deferred.create (fun read_from ->
    let socket =
      new%js WebSockets.webSocket
        (Js.string (sprintf "ws://%s:%d" host port))
    in
    socket##.onopen := Dom.handler (fun _event ->
      let reader =
        Pipe.create_reader ~close_on_exception:false (fun writer ->
          socket##.onmessage := Dom.handler (fun event ->
            Pipe.write_without_pushback writer event##.data;
            Js._false);
          Deferred.create (fun close_when ->
            socket##.onclose := Dom.handler (fun _event ->
              Ivar.fill close_when ();
              Js._false
            )
          )
        )
      in
      Ivar.fill read_from reader;
      Js._false))

let add_li ~to_:list_elt data =
  let document = Dom_html.window##.document in
  let li = document##createElement (Js.string "li") in
  Dom.appendChild li (document##createTextNode data);
  Dom.appendChild list_elt li

let main () =
  let broadcasts_list = Dom_html.getElementById "broadcasts" in
  read_from ~host:"localhost" ~port:20406
  >>= fun broadcasts ->
  Pipe.iter_without_pushback broadcasts ~f:(fun data ->
    add_li ~to_:broadcasts_list data)
  >>= fun () ->
  add_li ~to_:broadcasts_list (Js.string "lost connection");
  Deferred.unit

let () =
  Dom_html.window##.onload := Dom.handler (fun _event ->
    don't_wait_for (main ());
    Async_js.init ();
    Js._false)
