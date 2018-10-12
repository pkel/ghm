module Date = Date_yojson

module M = struct
  [@@@warning "-39"]

  type t = Date.t * Date.t [@@deriving compare, sexp, yojson]
end

include M

let of_dates t1 t2 = (Date.min t1 t2, Date.max t1 t2)

let cover (f1, t1) (f2, t2) = (Date.min f1 f2, Date.max t1 t2)

let nights (f, t) = Date.diff t f

let from (x, _) = x

let till (_, x) = x

let compare_by_from (a, _) (b, _) = Date.compare a b

let compare_by_till (_, a) (_, b) = Date.compare a b

let to_string_hum ?(sep = "bis") (f, t) =
  Format.asprintf "%a %s %a" Date.pp f sep Date.pp t
