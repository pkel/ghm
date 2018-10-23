open Core_kernel

type t [@@deriving compare, sexp, yojson]

(** Period between to dates. Order of arguments does no matter *)
val of_dates : Date.t -> Date.t -> t

val from : t -> Date.t
val till : t -> Date.t
val nights : t -> int

(** [cover a b] returns the minimal period that covers both [a] and [b] *)
val cover : t -> t -> t

val compare_by_from : t -> t -> int
val compare_by_till : t -> t -> int
val to_string_hum : ?sep:string -> t -> string
