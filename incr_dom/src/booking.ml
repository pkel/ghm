open Core_kernel

type t =
  { booking_id    : int option
  ; state         : string
  ; deposit_asked : float option
  ; deposit_got   : float option
  ; no_tax        : bool
  ; booking_note  : string
  ; company       : individual list
  ; rooms         : room list
  }

and individual =
  { given         : string
  ; second        : string
  ; family        : string
  ; date_of_birth : Date.t option
  }

and room =
  { room          : string
  ; beds          : int
  ; price_per_bed : float
  ; factor        : float
  ; description   : string
  ; breakfast     : bool
  ; from          : Date.t option
  ; to_           : Date.t option
  }
[@@deriving fields, compare]
