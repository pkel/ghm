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
  } [@@deriving fields, compare, sexp]
