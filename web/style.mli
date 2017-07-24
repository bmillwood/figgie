open Incr_dom
open Vdom

open Figgie

val suit_span : ?count:int -> gold:Card.Suit.t option -> Card.Suit.t -> Node.t

module Name : sig
  val colour : Username.t -> string

  val style : is_me:bool -> Username.t -> (string * string) list

  val span
    :  ?attrs:Attr.t list
    -> is_me:bool
    -> Username.t
    -> Node.t
end

module User : sig
  val gen      : is_me:bool -> _ Lobby.User.Gen.t    -> Node.t
  val observer : is_me:bool -> Lobby.User.Observer.t -> Node.t
end
