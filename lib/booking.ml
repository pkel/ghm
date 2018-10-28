open Core_kernel
module Date = Date_yojson

type t =
  { period : Period.t
  ; deposit_asked : float option
  ; deposit_got : float option
  ; tax_free : bool
  ; note : string
  ; guests : guest list
  ; allocs : alloc list }

and guest =
  { given : string
  ; family : string
  ; born : Date.t option }

and alloc =
  { room : string
  ; price_per_bed : float
  ; beds : int
  ; description : string }
[@@deriving yojson, fields, compare, sexp]

module Summary = struct
  type t =
    { rooms : string list
    ; guests : int
    ; tax_payers : int }
  [@@deriving fields]
end

let summarize (t : t) : Summary.t =
  (* Age on arrival >= 15 makes you a tax payer *)
  let tax_cutoff = Date.add_years (Period.till t.period) (-15) in
  let guests = List.length t.guests
  and tax_payers =
    List.count t.guests ~f:(fun g ->
        match g.born with None -> true | Some d -> Date.compare tax_cutoff d > 0 )
  and rooms =
    List.map t.allocs ~f:room
    |> List.filter ~f:(Fn.compose not String.is_empty)
    |> String.(List.dedup_and_sort ~compare)
  in
  {rooms; guests; tax_payers}
;;

let room_descriptions =
  [ "Einzelzimmer"
  ; "Doppelzimmer"
  ; "Dreibettzimmer"
  ; "Vierbettzimmer"
  ; "Doppel- als Einzelzimmer"
  ; "Kurtaxe" ]
;;
