open Core_kernel.Std

let room_size = 4

module Room = struct
  module Id : Identifiable.S = String
  type t = {
    players : Username.t list;
  } [@@deriving bin_io, sexp]

  let empty = { players = [] }
end

type t = Room.t Room.Id.Map.t
  [@@deriving bin_io, sexp]
