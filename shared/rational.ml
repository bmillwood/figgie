open Core_kernel

let gcd a b =
  let rec g a b =
    if b = 0
    then Int.abs a
    else g b (a mod b)
  in
  if a >= b then g a b else g b a

module T = struct
  module T' : sig
    type t = private { n : int; d : int } [@@deriving bin_io, hash]
    val create : n:int -> d:int -> t
  end = struct
    type t = { n : int; d : int } [@@deriving bin_io, hash]

    (* enforces d > 0 and gcd d n = 1 *)
    let create ~n ~d =
      let n, d =
        match Int.sign d with
        | Pos -> n, d
        | Neg -> Int.neg n, Int.neg d
        | Zero -> failwith "Rational.create ~d:0"
      in
      let g = gcd n d in
      { n = n / g; d = d / g }
  end
  include T'

  let compare x y = Int.compare (x.n * y.d) (y.n * x.d)

  let of_int n = create ~n ~d:1
  let to_float t = Float.of_int t.n /. Float.of_int t.d

  let sexp_of_t { n; d } =
    if d = 1
    then sexp_of_int n
    else if Int.O.(abs n < d)
    then Sexp.Atom (sprintf "%d/%d" n d)
    else begin
      let sign, n = if n < 0 then "-", Int.abs n else "", n in
      Sexp.List
        [ Sexp.Atom (sprintf "%s%d" sign (n / d))
        ; Sexp.Atom (sprintf "%d/%d" (n mod d) d)
        ]
    end

  let of_n_slash_d str =
    Option.map (String.lsplit2 str ~on:'/') ~f:(fun (before, after) ->
        create
          ~n:(Int.of_string before)
          ~d:(Int.of_string after)
      )

  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom atom ->
      begin match of_n_slash_d atom with
      | Some r -> r
      | None -> of_int (Int.t_of_sexp sexp)
      end
    | Sexp.List [int_part; Sexp.Atom frac_part] ->
      let frac =
        Option.value_exn (of_n_slash_d frac_part)
          ~error:(
            Error.create_s [%message
              "Rational.t_of_sexp: no / in fractional part"
                (sexp : Sexp.t)
                (frac_part : string)
            ])
      in
      create ~n:(frac.d * Int.t_of_sexp int_part + frac.n) ~d:frac.d
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
  let num t = t.n
  let den t = t.d
  let (+) t1 t2 = create ~n:(t1.n * t2.d + t2.n * t1.d) ~d:(t1.d * t2.d)
  let (-) t1 t2 = t1 + neg t2
  let ( * ) i t = create ~n:(t.n * i) ~d:t.d
  let ( / ) t i = let open Int.O in create ~n:t.n ~d:(t.d * i)
end
include O
