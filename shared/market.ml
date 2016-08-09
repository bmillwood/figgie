open Core_kernel.Std

module Cpty       = Username
module Symbol     = Card.Suit
module Per_symbol = struct
  include Card.Hand
  let get t ~symbol = get t ~suit:symbol
  let set t ~symbol = set t ~suit:symbol
  let modify t ~symbol = modify t ~suit:symbol
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

end

module Dirpair = struct
  type 'a t = { buy : 'a; sell : 'a } [@@deriving bin_io, sexp]

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

  let mapi t ~f =
    { buy = f Dir.Buy t.buy; sell = f Dir.Sell t.sell }
end

module type With_units = sig
  type t

  include Identifiable.S with type t := t

  val of_int : int -> t
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
  val more_agg          : t -> than:t -> dir:Dir.t -> bool
  val more_agg_or_equal : t -> than:t -> dir:Dir.t -> bool
end = struct
  include Rational

  let more_agg_or_equal t ~than ~dir =
    match (dir : Dir.t) with
    | Buy  -> t >= than
    | Sell -> t <= than

  let more_agg t ~than ~dir =
    match (dir : Dir.t) with
    | Buy  -> t > than
    | Sell -> t < than
end

module Size : sig 
  include With_units
  val to_int : t -> int
  val with_dir : t -> dir:Dir.t -> t
end = struct
  include Int
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

module Order = struct
  module Id : sig include Identifiable.S val zero : t val next : t -> t end = struct
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

  let more_agg_or_equal t ~than =
    Price.more_agg_or_equal t.price ~than:than.price ~dir:t.dir
  let more_agg t ~than =
    Price.more_agg t.price ~than:than.price ~dir:t.dir
end

module Exec = struct
  module Partial_fill = struct
    type t = {
      original_order : Order.t;
      filled_by : Size.t;
    } [@@deriving bin_io, sexp]
  end

  type t = {
    fully_filled     : Order.t list;
    partially_filled : Partial_fill.t option;
    posted           : Order.t option;
  } [@@deriving bin_io, sexp]
end

module Match_result = struct
  type 'a t = { exec : Exec.t; remaining : 'a }
end

module Halfbook = struct
  type t = Order.t list [@@deriving bin_io, sexp]

  let add_order t (order : Order.t) =
    let rec go acc (xs : t) =
      match xs with
      | o :: os when not (Order.more_agg order ~than:o) -> go (o :: acc) os
      | _ -> List.rev_append acc (order :: xs)
    in
    go [] t

  let cancel t (order : Order.t) =
    let rec go acc = function
      | [] -> Error `No_such_order
      | (o : Order.t) :: os ->
        if Order.Id.equal order.id o.id
        then Ok (List.rev_append acc os)
        else go (o :: acc) os
    in
    go [] t

  let rec match_ t order : t Match_result.t =
    match t with
    | o :: os when Order.more_agg_or_equal order ~than:o ->
      let open Size.O in
      begin match Ordering.of_int (Size.compare order.size o.size) with
      | Less ->
          let pf : Exec.Partial_fill.t =
            { original_order = o; filled_by = order.size }
          in
          { exec =
            { fully_filled = []
            ; partially_filled = Some pf
            ; posted = None
            }
          ; remaining = { o with size = o.size - order.size } :: os
          }
      | Equal ->
          { exec =
            { fully_filled = [o]
            ; partially_filled = None
            ; posted = None
            }
          ; remaining = os
          }
      | Greater ->
          let r = match_ os { order with size = order.size - o.size } in
          { r with exec = { r.exec with fully_filled = o :: r.exec.fully_filled } }
      end
    | _ ->
      { exec = { fully_filled = []; partially_filled = None; posted = Some order }
      ; remaining = t
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
          else r.remaining)
      | None -> Dirpair.set book ~dir:opp_dir ~to_:r.remaining
    in
    { r with remaining = Per_symbol.set t ~symbol:order.symbol ~to_:new_book }

  let cancel t (order : Order.t) =
    let book = Per_symbol.get t ~symbol:order.symbol in
    let same = Dirpair.get book ~dir:order.dir in
    Result.map (Halfbook.cancel same order) ~f:(fun hb ->
      Per_symbol.set t ~symbol:order.symbol
        ~to_:(Dirpair.set book ~dir:order.dir ~to_:hb))
end
