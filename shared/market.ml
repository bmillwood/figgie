open Core_kernel.Std

module Cpty       = Username
module Symbol     = Card.Suit
module Per_symbol = struct
  include Card.Hand
  let get t ~symbol = get t ~suit:symbol
  let set t ~symbol = set t ~suit:symbol
  let modify t ~symbol = modify t ~suit:symbol

  let zero ~zero = create_all zero
  let union_with = map2
end

module Dir = struct
  type t = Buy | Sell [@@deriving bin_io, compare, sexp]

  let equal x y = compare x y = 0

  let other = function | Buy -> Sell | Sell -> Buy
  let sign = function | Buy -> 1 | Sell -> -1

  let fold t ~buy ~sell =
    match t with
    | Buy -> buy
    | Sell -> sell

  let to_string = fold ~buy:"buy" ~sell:"sell"
end

module Dirpair = struct
  type 'a t = { buy : 'a; sell : 'a } [@@deriving bin_io, sexp]

  let init ~(f : Dir.t -> _) = { buy = f Buy; sell = f Sell }

  let create_both x = { buy = x; sell = x }

  let create_dir ~dir ~same ~opp =
    match (dir : Dir.t) with
    | Buy  -> { buy = same; sell = opp  }
    | Sell -> { buy = opp;  sell = same }

  let get t ~dir =
    match (dir : Dir.t) with
    | Buy  -> t.buy
    | Sell -> t.sell

  let set t ~dir ~to_ =
    match (dir : Dir.t) with
    | Buy  -> { t with buy  = to_ }
    | Sell -> { t with sell = to_ }

  let modify t ~dir ~f =
    match (dir : Dir.t) with
    | Buy  -> { t with buy  = f t.buy  }
    | Sell -> { t with sell = f t.sell }

  let map t ~f =
    init ~f:(fun dir -> f (get t ~dir))

  let mapi t ~f =
    init ~f:(fun dir -> f dir (get t ~dir))

  let map2 t1 t2 ~f =
    init ~f:(fun dir -> f (get t1 ~dir) (get t2 ~dir))
end

module type With_units = sig
  type t

  include Identifiable.S with type t := t

  val of_int : int -> t
  val to_rational : t -> Rational.t
  val to_float : t -> float

  module O : sig
    val zero : t
    val neg : t -> t
    val (+) : t -> t -> t
    val (-) : t -> t -> t
    val ( * ) : int -> t -> t
    val (/) : t -> int -> t
    val (=) : t -> t -> bool
    val (<>) : t -> t -> bool
    val (<) : t -> t -> bool
    val (>) : t -> t -> bool
    val (<=) : t -> t -> bool
    val (>=) : t -> t -> bool
  end
  include module type of O
end

module Price : sig
  include With_units

  val to_rational : t -> Rational.t

  val is_more_agg          : t -> than:t -> dir:Dir.t -> bool
  val is_more_agg_or_equal : t -> than:t -> dir:Dir.t -> bool

  val make_more_agg : t -> by:t -> dir:Dir.t -> t
end = struct
  include Rational

  let to_rational t = t

  let is_more_agg_or_equal t ~than ~dir =
    match (dir : Dir.t) with
    | Buy  -> t >= than
    | Sell -> t <= than

  let is_more_agg t ~than ~dir =
    match (dir : Dir.t) with
    | Buy  -> t > than
    | Sell -> t < than

  let make_more_agg t ~by ~dir =
    match (dir : Dir.t) with
    | Buy  -> t + by
    | Sell -> t - by
end

module Size : sig 
  include With_units
  val to_int : t -> int
  val with_dir : t -> dir:Dir.t -> t
end = struct
  include Int
  let to_rational t = Rational.of_int (to_int t)
  let with_dir t ~dir = t * Dir.sign dir
end

module O = struct
  let ( *$ ) size price =
    let size = Size.to_int size in
    Price.O.(size * price)
  module Price = Price.O
  module Size = Size.O
end
let ( *$ ) = O.( *$ )

module Positions = struct
  type t =
    { cash  : Price.t
    ; stuff : Size.t Per_symbol.t
    } [@@deriving sexp]

  let zero =
    { cash  = Price.zero
    ; stuff = Per_symbol.zero ~zero:Size.zero
    }

  let (+) t1 t2 =
    { cash = Price.(+) t1.cash t2.cash
    ; stuff = Per_symbol.union_with t1.stuff t2.stuff ~f:Size.(+)
    }

  let neg t =
    { cash = Price.neg t.cash
    ; stuff = Per_symbol.map t.stuff ~f:Size.neg
    }

  let (-) t1 t2 = t1 + neg t2

  let buy ~symbol ~size ~price =
    { cash  = size *$ Price.neg price
    ; stuff = Per_symbol.set zero.stuff ~symbol ~to_:size
    }

  let sell ~symbol ~size ~price = neg (buy ~symbol ~size ~price)

  let trade ~dir ~symbol ~size ~price =
    Dir.fold dir ~buy ~sell ~symbol ~size ~price
end

module Order = struct
  module Id : sig
    include Identifiable.S
    val zero : t
    val next : t -> t
  end = struct
    include Int
    let next t = t + 1
  end

  type t = {
    owner : Cpty.t;
    id : Id.t;
    symbol : Symbol.t;
    dir : Dir.t;
    price : Price.t;
    size : Size.t;
  } [@@deriving bin_io, sexp]

  let is_more_agg_or_equal t ~than =
    Price.is_more_agg_or_equal t.price ~than:than.price ~dir:t.dir
  let is_more_agg t ~than =
    Price.is_more_agg t.price ~than:than.price ~dir:t.dir
end

module Exec = struct
  module Partial_fill = struct
    type t = {
      original_order : Order.t;
      filled_by : Size.t;
    } [@@deriving bin_io, sexp]
  end

  type t = {
    order            : Order.t;
    fully_filled     : Order.t list;
    partially_filled : Partial_fill.t option;
    posted           : Order.t option;
  } [@@deriving bin_io, sexp]

  let fills t =
    match t.partially_filled with
    | None -> t.fully_filled
    | Some { original_order; filled_by } ->
      { original_order with size = filled_by } :: t.fully_filled

  let position_effect t =
    List.fold (fills t)
      ~init:Cpty.Map.empty
      ~f:(fun acc fill ->
          let crossing_price = fill.price in
          let symbol = fill.symbol in
          let size = fill.size in
          let update acc ~cpty ~f =
            Map.update acc cpty
              ~f:(fun p -> f (Option.value p ~default:Positions.zero))
          in
          let delta_for_filler =
            Positions.trade ~dir:t.order.dir
              ~symbol ~size ~price:crossing_price
          in
          let delta_for_fillee = Positions.neg delta_for_filler in
          acc
          |> update ~cpty:t.order.owner ~f:(Positions.(+) delta_for_filler)
          |> update ~cpty:fill.owner    ~f:(Positions.(+) delta_for_fillee)
        )
end

module Match_result = struct
  type 'a t =
    { exec : Exec.t
    ; new_market : 'a
    }
end

module Halfbook = struct
  type t = Order.t list [@@deriving bin_io, sexp]

  let add_order t (order : Order.t) =
    let rec go acc (xs : t) =
      match xs with
      | o :: os when not (Order.is_more_agg order ~than:o) -> go (o :: acc) os
      | _ -> List.rev_append acc (order :: xs)
    in
    go [] t

  let cancel t (order : Order.t) =
    let rec go acc = function
      | [] -> Error `No_such_order
      | (o : Order.t) :: os ->
        if Username.equal order.owner o.owner && Order.Id.equal order.id o.id
        then Ok (List.rev_append acc os)
        else go (o :: acc) os
    in
    go [] t

  let rec match_ t order : t Match_result.t =
    match t with
    | o :: os when Order.is_more_agg_or_equal order ~than:o ->
      let open Size.O in
      begin match Ordering.of_int (Size.compare order.size o.size) with
      | Less ->
          let pf : Exec.Partial_fill.t =
            { original_order = o; filled_by = order.size }
          in
          { exec =
            { order
            ; fully_filled = []
            ; partially_filled = Some pf
            ; posted = None
            }
          ; new_market = { o with size = o.size - order.size } :: os
          }
      | Equal ->
          { exec =
            { order
            ; fully_filled = [o]
            ; partially_filled = None
            ; posted = None
            }
          ; new_market = os
          }
      | Greater ->
          let r = match_ os { order with size = order.size - o.size } in
          let fully_filled = o :: r.exec.fully_filled in
          { r with exec = { r.exec with order; fully_filled } }
      end
    | _ ->
      { exec =
          { order
          ; fully_filled = []
          ; partially_filled = None
          ; posted = Some order
          }
      ; new_market = t
      }
end

module Book = struct
  type t = Halfbook.t Dirpair.t Per_symbol.t [@@deriving bin_io, sexp]

  let empty = Per_symbol.create_all (Dirpair.create_both [])

  let match_ t (order : Order.t) : t Match_result.t =
    let book = Per_symbol.get t ~symbol:order.symbol in
    let opp_dir = Dir.other order.dir in
    let opp_book = Dirpair.get book ~dir:opp_dir in
    let r = Halfbook.match_ opp_book order in
    let new_book =
      match r.exec.posted with
      | Some posted ->
        Dirpair.mapi book ~f:(fun side hbook ->
          if Dir.equal side order.dir
          then Halfbook.add_order hbook posted
          else r.new_market)
      | None -> Dirpair.set book ~dir:opp_dir ~to_:r.new_market
    in
    { r with new_market = Per_symbol.set t ~symbol:order.symbol ~to_:new_book }

  let cancel t (order : Order.t) =
    let book = Per_symbol.get t ~symbol:order.symbol in
    let same = Dirpair.get book ~dir:order.dir in
    Result.map (Halfbook.cancel same order) ~f:(fun hb ->
      Per_symbol.set t ~symbol:order.symbol
        ~to_:(Dirpair.set book ~dir:order.dir ~to_:hb))
end
