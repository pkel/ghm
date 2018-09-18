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
  ; breakfast     : bool
  ; nights        : (Date.t * Date.t) option
  }
[@@deriving fields, compare, sexp]
