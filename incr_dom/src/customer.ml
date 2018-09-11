open Core_kernel

type t =
  { customer_id      : int option

  ; title            : string
  ; title_letter     : string

  ; given            : string
  ; second           : string
  ; family           : string

  ; business         : string
  ; business_address : string

  ; street           : string
  ; street_number    : string
  ; postal_code      : string
  ; city             : string
  ; country          : string
  ; country_code     : string

  ; phone            : string
  ; phone2           : string
  ; mobile           : string
  ; fax              : string
  ; fax2             : string
  ; mail             : string
  ; mail2            : string
  ; web              : string

  ; keyword          : string
  ; note             : string

  ; bookings         : Booking.t list
  } [@@deriving fields, compare]
