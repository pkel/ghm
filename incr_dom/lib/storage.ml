open Core_kernel

type t = (Customer.t * Booking.t list) Int.Map.t [@@deriving compare, sexp]

let empty = Int.Map.empty
let save = Int.Map.set
let load = Int.Map.find
let size = Int.Map.length

let pp_hum fmt t =
  Sexplib.Sexp.pp_hum fmt (sexp_of_t t)

let of_string s =
  Sexp.of_string s |> t_of_sexp
