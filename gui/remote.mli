open Ghm

type 'a order = Asc of 'a | Desc of 'a

module Customer : sig
  type t = Customer.t

  type id = int

  val get : id -> (unit, t) Request.t

  val post : (t, id * t) Request.t

  val patch : id -> (t, t) Request.t
end

module Customers : sig
  type t = (int * Customer.t) list

  type key = Id | Modified

  val get :
    ?offset:int -> ?limit:int -> ?sort:key order -> unit -> (unit, t) Request.t
end
