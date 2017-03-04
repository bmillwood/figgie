open Core_kernel.Std

let host_and_port hps =
  match Host_and_port.of_string hps with
  | exception _ ->
    let ok_in_hostname c =
      List.exists ~f:(fun p -> p c)
        [ Char.is_alpha
        ; Char.is_digit
        ; Char.equal '-'
        ; Char.equal '.'
        ]
    in
    if not (String.is_empty hps)
      && String.for_all hps ~f:ok_in_hostname
      && not (String.is_prefix hps ~prefix:"-")
      && not (String.is_suffix hps ~suffix:"-")
    then
      Some (Host_and_port.create
          ~host:hps
          ~port:Protocol.default_websocket_port)
    else None
  | host_and_port -> Some host_and_port
