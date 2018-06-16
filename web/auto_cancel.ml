open Core_kernel.Std
open Async_kernel
open Async_rpc_kernel

open Figgie

type t =
  | Never
  | My_trades
  | Any_trades

let initial =
  match Url_vars.cancel_on with
  | Some "mytrades" -> My_trades
  | Some "anytrades" -> Any_trades
  | Some _ | None -> Never

let exec t ~(exec : Market.Exec.t) ~my_name ~conn =
  if Market.Exec.is_fill exec then begin
    let cancel_all () =
      don't_wait_for begin
        match%map Rpc.Rpc.dispatch_exn Protocol.Cancel_all.rpc conn () with
        | Error #Protocol.not_playing -> ()
        | Ok `Ack -> ()
      end
    in
    match t with
    | Never -> ()
    | Any_trades ->
        cancel_all ()
    | My_trades ->
        let is_mine (order : Market.Order.t) =
          Username.(equal my_name) order.owner
        in
        let involved_me =
          is_mine exec.order
          || List.exists (Market.Exec.fills exec) ~f:is_mine
        in
        if involved_me
        then cancel_all ()
  end
