open Core_kernel.Std

module Shortener = struct
  type t =
    { accept : bool
    ; trans  : t Char.Map.t
    }

  let empty =
    { accept = false
    ; trans  = Char.Map.empty
    }

  let add t s =
    let rec go cur i =
      if i = String.length s then (
        { cur with accept = true }
      ) else (
        let trans =
          Map.update cur.trans s.[i] ~f:(fun next ->
            let next = Option.value next ~default:empty in
            go next (i + 1))
        in
        { cur with trans }
      )
    in
    go t 0

  let of_list = List.fold ~init:empty ~f:add

  let short t s =
    let rec go cur acc i =
      if i = String.length s then (
        acc
      ) else (
        let c = s.[i] in
        let acc =
          if not cur.accept && Map.is_empty (Map.remove cur.trans c)
          then acc
          else c :: acc
        in
        match Map.find cur.trans c with
        | None -> acc
        | Some next -> go next acc (i + 1)
      )
    in
    String.of_char_list (List.rev (go t [] 0))
end

include String
