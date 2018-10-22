open Core_kernel
module Date = Date_yojson

type t =
  { period: Period.t
  ; deposit_asked: float option
  ; deposit_got: float option
  ; note: string
  ; guests: guest list
  ; rooms: room list }

and guest = {given: string; second: string; family: string; born: Date.t option}

and room =
  { room: string
  ; beds: int
  ; price_per_bed: float
  ; factor: float
  ; description: string }
[@@deriving yojson, fields, compare, sexp]

module Summary = struct
  type t = {rooms: int; beds: int; guests: int; period: Period.t}
  [@@deriving fields]
end

let summarize (t : t) : Summary.t =
  let guests = List.length t.guests
  and rooms, beds =
    List.fold_left t.rooms ~init:(0, 0) ~f:(fun (r, b) e -> (r + 1, b + e.beds))
  and period = t.period in
  {rooms; guests; period; beds}
