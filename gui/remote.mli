open Ghm

module Login : sig
  type credentials =
    { user : string
    ; pass : string }
  [@@deriving compare]

  type token = string [@@deriving compare]

  val get_token : (credentials, token) Request.t
end

type 'a order =
  | Asc of 'a
  | Desc of 'a

module Customer : sig
  type t = Customer.t
  type id = int

  val get : id -> (unit, t) Request.t
  val post : (t, id * t) Request.t
  val patch : id -> (t, t) Request.t
  val delete : id -> (unit, unit) Request.t
end

module Customers : sig
  type t = (int * Customer.t) list

  type key =
    | Id
    | Modified

  type filter = Keyword of string

  val get 
    :  ?offset:int
    -> ?limit:int
    -> ?sort:key order list
    -> ?filter:filter
    -> unit
    -> (unit, t) Request.t
end
