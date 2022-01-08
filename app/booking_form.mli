open Incr_dom

type env = { lock_invoice : bool Incr.t }

include Interfaces.EDITABLE with type env := env and type t := Ghm.Booking.t
