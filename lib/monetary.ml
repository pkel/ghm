open Core_kernel

type t = int [@@deriving compare, sexp]

let zero = 0

type remote = float [@@deriving yojson]

let of_int x = 100 * x
let of_float x = Float.iround ~dir:`Nearest (x *. 100.)
let to_float x = Float.of_int x /. 100.
let cents x = x

let of_yojson x =
  match remote_of_yojson x with
  | Error e -> Error e
  | Ok f ->
    (match of_float f with
    | Some x -> Ok x
    | None -> Error "could not read float")
;;

let to_yojson x = remote_to_yojson (to_float x)
let ( + ) = ( + )
let ( - ) = ( - )
let times i x = i * x

let print' sep fmt x =
  let rest = x mod 100
  and lead = x / 100 in
  Format.fprintf fmt "%d%s%02d" lead sep rest
;;

let print = print' ","
let print_dot = print' "."
let to_string x = Format.asprintf "%a" print x
let to_string_dot x = Format.asprintf "%a" print_dot x
