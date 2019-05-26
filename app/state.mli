open Base

type error =
  { gist : string
  ; detail : Error.t
  }
[@@deriving sexp_of, compare]

type t =
  { handle_error : error -> unit
  ; connection : Remote.connection
  }
