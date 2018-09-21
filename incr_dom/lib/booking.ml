open Core_kernel

type t =
  { deposit_asked : float option
  ; deposit_got   : float option
  ; no_tax        : bool
  ; note          : string
  ; guests        : guest list
  ; rooms         : room list
  }

and guest =
  { given         : string
  ; second        : string
  ; family        : string
  ; born          : Date.t option
  }

and room =
  { room          : string
  ; beds          : int
  ; price_per_bed : float
  ; factor        : float
  ; description   : string
  ; period        : Period.t
  }
[@@deriving fields, compare, sexp]

type summary =
  { rooms  : int
  ; beds   : int
  ; guests : int
  ; period : Period.t option
  }

let summarize (t: t) : summary =
  let guests = List.length t.guests
  and rooms, beds, period =
    List.fold_left t.rooms
      ~init:(0,0,None)
      ~f:(fun (r, b, p) e ->
          ( r + 1
          , b + e.beds
          , Some Option.(map ~f:(Period.cover e.period) p
                         |> value ~default:e.period)))
  in {rooms; guests; period; beds}
