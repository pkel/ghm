module Date = Date_yojson

module M = struct
  [@@@warning "-39"]

  type t = Date.t * Date.t [@@deriving compare, sexp, yojson]
end

include M

let a (x, _) = x
let b (_, x) = x
let from (x, y) = Date.min x y
let till (x, y) = Date.max x y
let of_dates x y = x, y
let cover d1 d2 = Date.min (from d1) (from d2), Date.max (from d1) (from d2)
let nights d = Date.diff (till d) (from d)
let compare_by_from d1 d2 = Date.compare (from d1) (from d2)
let compare_by_till d1 d2 = Date.compare (from d1) (from d2)

let update ?a ?b (x, y) =
  match a, b with
  | None, None -> x, y
  | Some x, Some y -> x, y
  | Some x, _ -> x, y
  | _, Some y -> x, y
;;

let to_string_hum ?(sep = "bis") d =
  let f, t = from d, till d in
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
