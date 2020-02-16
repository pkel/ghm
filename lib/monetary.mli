type t [@@deriving yojson, compare, sexp]

val zero : t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val times : int -> t -> t
val of_int : int -> t
val of_float : float -> t option
val to_float : t -> float
val cents : int -> t

(** German notation, i.e. 3,49 *)
val to_string : t -> string

val to_string_dot : t -> string
val print : Format.formatter -> t -> unit
val print_dot : Format.formatter -> t -> unit
