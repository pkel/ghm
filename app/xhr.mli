open Core_kernel
open Async_kernel
include Postgrest.REQUEST with type body = string

type connection

(** Connection takes care of authentication and token renewal. Create once and
    store in global state. Fails if token cannot be retrieved. *)
val connect : unit -> connection option Deferred.t

val send
  :  c:connection
  -> body:'a
  -> handler:('b Or_error.t -> unit)
  -> ('a, 'b) t
  -> unit

val send' : c:connection -> handler:('b Or_error.t -> unit) -> (unit, 'b) t -> unit

module Deferred : sig
  open Async_kernel

  val send : c:connection -> body:'a -> ('a, 'b) t -> 'b Or_error.t Deferred.t
  val send' : c:connection -> (unit, 'b) t -> 'b Or_error.t Deferred.t
end
