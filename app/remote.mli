open Ghm
open Async_kernel

type connection
type ('a, 'b) request

(** Connection takes care of authentication and token renewal. Create once and
    store in global state. Fails if token cannot be retrieved. *)
val connect : unit -> connection option Deferred.t

(** Lift an unauthenticated request to a complete request with JWT token.
    Since JWT tokens expiry, this should happen right before sending the
    request. *)
val finalize : connection -> ('a, 'b) request -> ('a, 'b) Request.t

type 'a order =
  | Asc of 'a
  | Desc of 'a

module Customer : sig
  type t = Customer.t
  type id = int

  val get : id -> (unit, t) request
  val post : (t, id * t) request
  val patch : id -> (t, t) request
  val delete : id -> (unit, unit) request
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
    -> unit
    -> (unit, t) request
end
