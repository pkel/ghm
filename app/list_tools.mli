open Incr_dom

type action [@@deriving sexp]

val apply_action : 'a list -> int -> action -> 'a list

open Vdom

val view : inject:(action -> Event.t) -> string -> Node.t list
