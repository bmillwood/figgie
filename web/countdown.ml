open Core_kernel.Std
open Async_kernel

module Model = struct
  type t = {
    end_time : Time_ns.t sexp_opaque;
    hr  : int;
    min : int;
    sec : int;
    tick_event : (unit, unit) Clock_ns.Event.t;
  } [@@deriving sexp_of]

  let to_string { hr; min; sec; _ } =
    if hr > 0
    then sprintf "%d:%02d:%02d" hr min sec
    else sprintf "%02d:%02d" min sec
end

module Action = struct
  type t = Tick [@@deriving sexp_of]

  let of_end_time ~schedule end_time =
    let this_time =
      Time_ns.add
        (Time_ns.now ())
        (Time_ns.Span.of_sec 0.5)
    in
    let remaining = Time_ns.diff end_time this_time in
    let { Time_ns.Span.Parts.hr; min; sec; _ } =
      let parts = Time_ns.Span.to_parts remaining in
      match parts.sign with
      | Pos | Zero -> parts
      | Neg -> Time_ns.Span.(to_parts zero)
    in
    let tick_event =
      let next_tick_time =
        Time_ns.sub end_time
          (Time_ns.Span.create ~hr ~min ~sec ())
      in
      let event = Clock_ns.Event.at next_tick_time in
      upon (Clock_ns.Event.fired event)
        (function
        | Aborted  () -> ()
        | Happened () -> schedule Tick);
      event
    in
    { Model.end_time; hr; min; sec; tick_event }

  let apply Tick ~schedule (clock : Model.t) =
    if Time_ns.(now () >= clock.end_time)
    (* probably redundant, but will return sanity in more cases *)
    then { clock with hr = 0; min = 0; sec = 0 }
    else of_end_time ~schedule clock.end_time
end

let of_end_time = Action.of_end_time
