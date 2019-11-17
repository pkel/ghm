open Core_kernel
include Postgrest.REQUEST with type body = string

type connection

(** Connection takes care of authentication and token renewal. Create once and
    store in global state. Fails if token cannot be retrieved. *)
val connect
  :  base_url:string
  -> username:string
  -> password:string
  -> connection Or_error.t Lwt.t

val send
  :  c:connection
  -> body:'a
  -> ('a, 'b) t
  -> (Cohttp.Response.t * 'b Or_error.t) Lwt.t

val send' : c:connection -> (unit, 'b) t -> (Cohttp.Response.t * 'b Or_error.t) Lwt.t
