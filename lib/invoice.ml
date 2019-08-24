open Core_kernel
module Date = Date_yojson

type t =
  { id : string option
  ; recipient : string
  ; date : Date.t
  ; title : string
  ; intro : string
  ; positions : position list
  ; closing : string
  ; deposit : Monetary.t
  }

and position =
  { quantity : int
  ; description : string
  ; price : Monetary.t
  ; tax : int (** percent *)
  }
[@@deriving yojson, fields, compare, sexp]

let empty_position = { quantity = 1; description = ""; price = Monetary.zero; tax = 19 }

let empty date =
  { recipient = ""
  ; title = ""
  ; id = None
  ; date
  ; positions = []
  ; deposit = Monetary.zero
  ; intro = ""
  ; closing = ""
  }
;;

type summary =
  { sum : Monetary.t
  ; included_tax : (int * Monetary.t) list
  }

let summary : t -> summary =
  let add tax x txmap =
    let was = Int.Map.find txmap tax |> Option.value ~default:0.
    and tax' = float_of_int tax /. 100. in
    let data = was +. (x *. tax' /. (1. +. tax')) in
    Int.Map.set ~key:tax ~data txmap
  and monetary x = Monetary.of_float x |> Option.value_exn in
  fun t ->
    let sum, txmap =
      List.fold_left
        ~f:(fun (sum, txmap) pos ->
          let x = Monetary.(times pos.quantity pos.price |> to_float) in
          sum +. x, add pos.tax x txmap)
        ~init:(0., Int.Map.empty)
        t.positions
    in
    { sum = monetary sum; included_tax = Int.Map.(map ~f:monetary txmap |> to_alist) }
;;

let of_customer_and_booking date (c : Customer.t) (b : Booking.t) =
  let nights = Period.nights b.period
  and s = Booking.summarize b
  and describe (a : Booking.alloc) =
    Printf.sprintf "Übernachtung im %s" a.description
  in
  let title = "Rechnung"
  and intro =
    Printf.sprintf
      "Für Ihren Aufenthalt vom %s stellen wir die folgenden Positionen in Rechnung."
      (Period.to_string_hum b.period)
  and recipient =
    Printf.sprintf
      "%s %s\n%s\n%s-%s %s"
      c.name.given
      c.name.family
      c.address.street_with_num
      c.address.country_code
      c.address.postal_code
      c.address.city
  and positions =
    let eaters, positions =
      List.fold_right b.allocs ~init:(0, []) ~f:(fun a (b, p) ->
          ( a.beds + b
          , { quantity = a.beds
            ; description = describe a
            ; price = Monetary.(a.price_per_bed - of_int 5)
            ; tax = 7
            }
            :: p ))
    in
    (positions
    @ (if eaters > 0
      then
        [ { quantity = eaters
          ; price = Monetary.of_int 5
          ; tax = 19
          ; description = "Frühstück"
          }
        ]
      else [])
    @
    if s.tax_payers > 0 && not b.tax_free
    then
      [ { quantity = s.tax_payers
        ; price = Monetary.of_int 2
        ; description = "Kurtaxe"
        ; tax = 7
        }
      ]
    else [])
    |> List.map ~f:(fun p -> { p with quantity = p.quantity * nights })
  and deposit = Option.value ~default:Monetary.zero b.deposit_got
  and closing =
    "Wir danken für Ihren Besuch und freuen uns auf Ihren nächsten Aufenthalt."
  in
  { recipient; title; id = None; date; positions; deposit; intro; closing }
;;
