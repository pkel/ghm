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

module type RESOURCE = sig
  type t
  type id
  type key
  type filter

  module S : sig
    val get : id -> (unit, t) request
    val post : (t, id * t) request
    val patch : id -> (t, t) request
    val delete : id -> (unit, unit) request
  end

  module M : sig
    type nonrec t = (id * t) list

    val get
      :  ?offset:int
      -> ?limit:int
      -> ?sort:key order list
      -> ?filter:filter
      -> unit
      -> (unit, t) request
  end
end

type customer_key =
  | Id
  | Modified

type customer_filter = Keyword of string

module Customer :
  RESOURCE
  with type t := Customer.t
   and type id := int
   and type key := customer_key
   and type filter := customer_filter

type booking_key =
  | Id
  | Modified
  | Arrival
  | Departure

type booking_filter = None

module Booking :
  RESOURCE
  with type t := Booking.t
   and type id := int
   and type key := booking_key
   and type filter := booking_filter
