open Core_kernel

type t [@@deriving compare, sexp, yojson]

(** Period between to dates. Order of arguments does not matter *)
val of_dates : Date.t -> Date.t -> t

val from : t -> Date.t
val till : t -> Date.t
val nights : t -> int

(** Extract the two dates, preserving their order. *)
val a : t -> Date.t

val b : t -> Date.t
val update : ?a:Date.t -> ?b:Date.t -> t -> t

(** [cover a b] returns the minimal period that covers both [a] and [b] *)
val cover : t -> t -> t

val compare_by_from : t -> t -> int
val compare_by_till : t -> t -> int
val to_string_hum : ?sep:string -> t -> string
