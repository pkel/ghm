module Date = Date_yojson

module M = struct
  [@@@warning "-39"]

  type t = Date.t * Date.t [@@deriving compare, sexp, yojson]
end

include M

let of_dates t1 t2 = Date.min t1 t2, Date.max t1 t2
let cover (f1, t1) (f2, t2) = Date.min f1 f2, Date.max t1 t2
let nights (f, t) = Date.diff t f
let from (x, _) = x
let till (_, x) = x
let compare_by_from (a, _) (b, _) = Date.compare a b
let compare_by_till (_, a) (_, b) = Date.compare a b

let to_string_hum ?(sep = "bis") (f, t) =
  let open Date in
  let open Format in
  let df = day f
  and dt = day t
  and mf = month f |> Core_kernel.Month.to_int
  and mt = month t |> Core_kernel.Month.to_int
  and yf = year f
  and yt = year t in
  let t = sprintf "%i.%i.%i" dt mt yt in
  let d = Core_kernel.Int.equal df dt
  and m = Core_kernel.Int.equal mf mt
  and y = Core_kernel.Int.equal yf yt in
  match d, m, y with
  | true, true, true -> t
  | false, true, true -> sprintf "%i. %s %s" df sep t
  | false, false, true -> sprintf "%i.%i. %s %s" df mf sep t
  | _ -> sprintf "%i.%i.%i %s %s" df mf yf sep t
;;
