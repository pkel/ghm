open Incr_dom
open Ghm

type env =
  { nav : Nav.booking Incr.t
  ; rel : Nav.booking -> Nav.main
  ; customer : Customer.t Incr.t
  ; customer_id : Nav.noi Incr.t
  }

include Interfaces.EDITABLE with type env := env and type t = Booking.t
