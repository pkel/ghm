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

let chunks size t =
  if size <= 0 then raise (Invalid_argument "size must be grater zero");
  let l,_,c = Int.Map.fold_right t ~f:(fun ~key ~data (l, sz, c) ->
      if sz = 0 then
        (c :: l, size, Int.Map.singleton key data)
      else
        (l, sz - 1, save c ~key ~data))
    ~init:([],size,empty)
  in c :: l

let append t c =
  Int.Map.append ~lower_part:t ~upper_part:c
