open Core_kernel.Std

let room_size = 4

module Room = struct
  module Id : Identifiable.S = String
  type t = {
    players : Username.t list;
  } [@@deriving bin_io, sexp]

  let empty = { players = [] }

  let has_player t ~username =
    List.mem t.players username ~equal:Username.equal

  let add_player t ~username =
    { players = username :: t.players }

  let is_full t = List.length t.players >= room_size
end

type t = Room.t Room.Id.Map.t
  [@@deriving bin_io, sexp]
