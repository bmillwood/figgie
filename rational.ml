open Core.Std

let gcd a b =
  let rec g a b =
    if b = 0
    then a
    else g b (a mod b)
  in
  if a >= b then g a b else g b a

module T = struct
  module T' : sig
    type t = private { n : int; d : int } [@@deriving bin_io, compare]
    val create : n:int -> d:int -> t
  end = struct
    type t = { n : int; d : int } [@@deriving bin_io, compare]

    let create ~n ~d =
      let g = gcd n d in
      { n = n / g; d = d / g }
  end
  include T'

  let of_int n = create ~n ~d:1

  let hash = Hashtbl.hash

  let sexp_of_t { n; d } =
    if d = 1
    then sexp_of_int n
    else Sexp.List [sexp_of_int (n / d); Sexp.Atom (sprintf "%d/%d" (n mod d) d)]

  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom _ -> of_int (int_of_sexp sexp)
    | Sexp.List [int_part; Sexp.Atom frac_part] ->
      begin match String.index frac_part '/' with
      | None -> failwith "no / in fractional part"
      | Some i ->
        let l = String.length frac_part in
        let d = int_of_string (String.sub frac_part ~pos:(i + 1) ~len:(l - i - 1)) in
        let n = int_of_string (String.sub frac_part ~pos:0 ~len:i) in
        let n = int_of_sexp int_part * d + n in
        create ~n ~d
      end
    | Sexp.List _ -> failwith "expected atom or pair"

  let to_string t = sexp_of_t t |> Sexp.to_string
  let of_string s = Sexp.of_string s |> t_of_sexp

  let module_name = "Rational"
end

module O = struct
  include T
  include Identifiable.Make(T)

  let zero = of_int 0
  let neg t = create ~n:(Int.neg t.n) ~d:t.d
  let (+) t1 t2 = create ~n:(t1.n * t2.d + t2.n * t1.d) ~d:(t1.d * t2.d)
  let (-) t1 t2 = t1 + neg t2
  let ( * ) i t = create ~n:(t.n * i) ~d:t.d
  let ( / ) t i = let open Int.O in create ~n:t.n ~d:(t.d * i)
end
include O
