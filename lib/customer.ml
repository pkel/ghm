open Core_kernel

[@@@warning "-39"]

module Name = struct
  type t = {title: string; letter: string; given: string; family: string}
  [@@deriving fields, compare, sexp, yojson]
end

module Address = struct
  type t =
    { street_with_num: string
    ; postal_code: string
    ; city: string
    ; country: string
    ; country_code: string }
  [@@deriving fields, compare, sexp, yojson]
end

module Contact = struct
  type t =
    { phone: string
    ; phone2: string
    ; mobile: string
    ; fax: string
    ; fax2: string
    ; mail: string
    ; mail2: string
    ; web: string }
  [@@deriving fields, compare, sexp, yojson]
end

module Company = struct
  type t = {name: string; address: string}
  [@@deriving fields, compare, sexp, yojson]
end

type t =
  { name: Name.t
  ; company: Company.t
  ; address: Address.t
  ; contact: Contact.t
  ; keyword: string
  ; note: string
  ; bookings: Booking.t list }
[@@deriving fields, compare, sexp, yojson]

let first_booking t : Booking.t option =
  match t.bookings with [] -> None | hd :: _ -> Some hd

let empty =
  { name= {title= ""; letter= ""; given= ""; family= ""}
  ; company= {name= ""; address= ""}
  ; address=
      { street_with_num= ""
      ; postal_code= ""
      ; city= ""
      ; country= ""
      ; country_code= "" }
  ; contact=
      { phone= ""
      ; phone2= ""
      ; mobile= ""
      ; fax= ""
      ; fax2= ""
      ; mail= ""
      ; mail2= ""
      ; web= "" }
  ; keyword= ""
  ; note= ""
  ; bookings= [] }
