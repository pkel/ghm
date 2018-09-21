open Core_kernel

type t [@@deriving compare, sexp]

val of_dates: Date.t -> Date.t -> t
(** Period between to dates. Order of arguments does no matter *)

val from: t -> Date.t
val till: t -> Date.t

val nights: t -> int

val cover: t -> t -> t
(** [cover a b] returns the minimal period that covers both [a] and [b] *)

val compare_by_from: t -> t -> int
val compare_by_till: t -> t -> int
