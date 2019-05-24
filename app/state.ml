open Base

type error =
  { gist : string
  ; detail : Error.t
  }
[@@deriving sexp_of, compare]

type t = { handle_error : error -> unit }

let log_error t e = t.handle_error e
let create ~handle_error = { handle_error }

(* TODO: Auth token should also be here, I guess. *)
