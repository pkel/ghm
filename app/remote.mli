open Ghm

module Auth : sig
  type token [@@deriving compare, sexp_of]

  val get_token : (unit, token) Request.t
  val invalid_token : token
  val username : token -> string
end

type 'a order =
  | Asc of 'a
  | Desc of 'a

type ('a, 'b) authenticated_request = Auth.token -> ('a, 'b) Request.t

module Customer : sig
  type t = Customer.t
  type id = int

  val get : id -> (unit, t) authenticated_request
  val post : (t, id * t) authenticated_request
  val patch : id -> (t, t) authenticated_request
  val delete : id -> (unit, unit) authenticated_request
end

module Customers : sig
  type t = (int * Customer.t) list

  type key =
    | Id
    | Modified

  type filter = Keyword of string [@@deriving compare]

  val get 
    :  ?offset:int
    -> ?limit:int
    -> ?sort:key order list
    -> ?filter:filter
    -> (unit, t) authenticated_request
end
