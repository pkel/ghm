open Core_kernel

type t =
  { title           : string
  ; title_letter    : string

  ; given           : string
  ; second          : string
  ; family          : string

  ; company         : string
  ; company_address : string

  ; street          : string
  ; street_number   : string
  ; postal_code     : string
  ; city            : string
  ; country         : string
  ; country_code    : string

  ; phone           : string
  ; phone2          : string
  ; mobile          : string
  ; fax             : string
  ; fax2            : string
  ; mail            : string
  ; mail2           : string
  ; web             : string

  ; keyword         : string
  ; note            : string

  ; bookings        : Booking.t list
  } [@@deriving fields, compare, sexp]

let first_booking t : Booking.t option =
  match t.bookings with
  | [] -> None
  | hd :: _ -> Some hd

let empty =
  { title = ""
  ; title_letter = ""

  ; given = ""
  ; second = ""
  ; family = ""

  ; company = ""
  ; company_address = ""

  ; street = ""
  ; street_number = ""
  ; postal_code = ""
  ; city = ""
  ; country = ""
  ; country_code = ""

  ; phone = ""
  ; phone2 = ""
  ; mobile = ""
  ; fax = ""
  ; fax2 = ""
  ; mail = ""
  ; mail2 = ""
  ; web = ""

  ; keyword = ""
  ; note = ""

  ; bookings = []
  }
