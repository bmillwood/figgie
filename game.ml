open Core.Std

module Player = struct
  type t = {
    mutable is_ready : bool;
    mutable username : Protocol.Username.t;
    mutable hand  : int Card.Hand.t;
    mutable chips : int;
  }

  let create ~username =
    { is_ready = false
    ; username
    ; hand  = Card.Hand.create_all 0
    ; chips = 0
    }
end

module Stage = struct
  type t =
    | Waiting_for_players
    | Playing
end

type t = {
  stage : Stage.t;
  players : Player.t Protocol.Username.Table.t;
}

let create () =
  { stage   = Waiting_for_players
  ; players = Protocol.Username.Table.create ()
  }

let num_players t = Hashtbl.length t.players

let player t ~username =
  Hashtbl.find_or_add t.players username
    ~default:(fun () -> Player.create ~username)

let set_ready t ~(player : Player.t) ~is_ready =
  match t.stage with
  | Playing -> Error `Already_playing
  | Waiting_for_players ->
      player.is_ready <- is_ready;
      if is_ready
          && num_players t >= Params.min_players
          && Hashtbl.for_all t.players ~f:(fun player -> player.is_ready)
      then Ok `All_ready
      else Ok `Still_waiting

let waiting_for t =
  let waiting_for_connects = Int.max 0 (Params.min_players - num_players t) in
  let waiting_for_readies = Hashtbl.count t.players ~f:(fun p -> not p.is_ready) in
  waiting_for_connects + waiting_for_readies

let deal t =
  let hands = Card.Hand.deal ~num_players:(num_players t) in
  let ix = ref 0 in
  Hashtbl.iteri t.players ~f:(fun ~key:_ ~data:player ->
    player.hand <- hands.(!ix);
    incr ix)      
