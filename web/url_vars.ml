open Core_kernel.Std

let from_query_string key =
  List.Assoc.find Url.Current.arguments
    ~equal:String.Caseless.equal key

let prefill_connect_to = from_query_string "address"

let auto_connect_to =
  Option.bind (from_query_string "autoconnect")
    ~f:Parse.host_and_port

let username = from_query_string "username"
