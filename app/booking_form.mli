open Incr_dom
open Ghm
open Interfaces

type env =
  { nav : Nav.booking Incr.t
  ; rel : Nav.booking -> Nav.main
  ; customer : Customer.t Incr.t
  ; new_booking : Booking.t inject
  ; delete_booking : unit inject
  }

include Interfaces.FORM_COMPONENT with type edit := Booking.t and type env := env
