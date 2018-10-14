open Yojson.Safe
open Core_kernel

(** A request with body ['a] and response type ['b]. *)
type ('a, 'b) t

type verb = GET | POST | PUT | PATCH | DELETE

val create : v:verb -> url:string -> (unit, string) t

val give_json : (unit, 'a) t -> (json, 'a) t

val give_text : (unit, 'a) t -> (string, 'a) t

val want_json : ('a, string) t -> ('a, json) t

val prefer : string -> ('a, 'b) t -> ('a, 'b) t
(** Set the prefer header. *)

val conv_resp : f:('b -> 'c Or_error.t) -> ('a, 'b) t -> ('a, 'c) t

val map_resp : f:('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

val map_body : f:('c -> 'a) -> ('a, 'b) t -> ('c, 'b) t

val send : body:'a -> handler:('b Or_error.t -> unit) -> ('a, 'b) t -> unit

val send' : handler:('b Or_error.t -> unit) -> (unit, 'b) t -> unit
