open Core.Std
open Async.Std

let param =
  let open Command.Param in
  flag "-username" (required (Arg_type.create Username.of_string))
    ~doc:"USER your username"

type cmd =
  | Ready
  | Hand
  | Book
  | Order of Market.Order.t
  | Cancel of Market.Order.Id.t
  [@@deriving sexp]

module type Simple_rpc = sig
  type query [@@deriving sexp]
  type response [@@deriving sexp]
  val rpc : (query, response) Rpc.Rpc.t
end

let command =
  Client.make_command
    ~summary:"Enter sexps manually"
    ~param
    ~username:Fn.id
    ~f:(fun client _username ->
      let say ?(on=Writer.stdout) s =
        Writer.write_sexp
          ~hum:true ~terminate_with:Newline (Lazy.force on)
          s
      in
      let saye s = say ~on:Writer.stderr [%sexp (s : string)] in
      Pipe.iter_without_pushback client.updates ~f:(fun update ->
        say [%sexp (update : Protocol.Player_update.t)])
      |> don't_wait_for;
      Pipe.iter (Reader.read_sexps (Lazy.force Reader.stdin))
        ~f:(fun sexp ->
          let do_rpc (type query)
            (module R : Simple_rpc with type query = query)
            (query : query) =
            Rpc.Rpc.dispatch_exn R.rpc client.conn query
            >>| R.sexp_of_response
            >>| say
          in
          match cmd_of_sexp sexp with
          | exception _ ->
              saye "?";
              Deferred.unit
          | Ready -> do_rpc (module Protocol.Is_ready) true
          | Hand -> do_rpc (module Protocol.Hand) ()
          | Book -> do_rpc (module Protocol.Book) ()
          | Order order -> do_rpc (module Protocol.Order) order
          | Cancel id -> do_rpc (module Protocol.Cancel) id))
