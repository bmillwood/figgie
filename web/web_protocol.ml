open Core_kernel.Std

module Message = struct
  type t =
    | Broadcast of string
    [@@deriving bin_io]
end
