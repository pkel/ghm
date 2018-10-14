open Ghm

type 'a order = Asc of 'a | Desc of 'a

(** TODO: simplify paging to 'a list * (unit -> unit) option *)
type 'a page = Last of 'a list | More of 'a list * (unit, 'a page) Request.t

module Customer : sig
  type t = Customer.t

  type id = int

  val get : id -> (unit, t) Request.t

  val post : (t, id * t) Request.t

  val patch : id -> (t, t) Request.t
end

module Customers : sig
  type t = Customer.id * Customer.t

  type key = Id | Modified

  val get_page : ?sort:key order -> n:int -> (unit, t page) Request.t
end
