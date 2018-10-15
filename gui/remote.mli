open Ghm
open Core_kernel

type 'a order = Asc of 'a | Desc of 'a

type 'a page = {this: 'a; is_first: bool; load_next: (unit -> unit) option}

module Customer : sig
  type t = Customer.t

  type id = int

  val get : id -> (unit, t) Request.t

  val post : (t, id * t) Request.t

  val patch : id -> (t, t) Request.t
end

module Customers : sig
  type t = Customer.t Int.Map.t

  type key = Id | Modified

  val get_page : ?sort:key order -> n:int -> (unit, t page) Request.t
end
