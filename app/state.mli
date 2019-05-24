open Base

type t

type error =
  { gist : string
  ; detail : Error.t
  }
[@@deriving sexp_of, compare]

val log_error : t -> error -> unit
val create : handle_error:(error -> unit) -> t
