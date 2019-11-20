open Incr_dom
open Ghm
open Interfaces

type env =
  { nav : Nav.booking Incr.t
  ; rel : Nav.booking -> Nav.main
  ; customer : Customer.t Incr.t
  ; customer_id : Nav.noi Incr.t
  }

include FORM_COMPONENT with type edit := Booking.t and type env := env
