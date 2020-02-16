open Core_kernel
open Invoice

let id x =
  match x.date with
  | None -> "n/a"
  | Some d ->
    let d = Date.day d
    and m = Date.month d |> Month.to_int
    and y = Date.year d mod 2000 in
    sprintf "%i%i%i" y m d
;;

let tax_unit_default = Monetary.cents 200

(* TODO: it might be nice to split the period and handle kurtaxe on a
 * daily basis. *)
let tax_unit date =
  let m = Date.month date |> Core_kernel.Month.to_int in
  (* https://www.reichenau-tourismus.de/de/planen-buchen/kurtaxe-gaestekarte *)
  if m < 4 || m > 10 then Monetary.cents 80 else tax_unit_default
;;

let gen ?date (c : Customer.t) (b : Booking.t) =
  let nights = Period.nights b.period
  and s = Booking.Summary.of_booking b
  and describe (a : Booking.alloc) = Printf.sprintf "Übernachtung im %s" a.description in
  let title = "Rechnung"
  and id, tax_unit =
    match Booking.Rooms.first s.rooms with
    | Some room ->
      let date = Period.till b.period in
      let open Date in
      let d = day date
      and m = month date |> Core_kernel.Month.to_int
      and y = year date % 1000 in
      let s = sprintf "%02i%02i%02i-%s" y m d room in
      Some s, tax_unit date
    | _ -> None, tax_unit_default
  in
  let intro =
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
      [ { quantity = s.tax_payers; price = tax_unit; description = "Kurtaxe"; tax = 7 } ]
    else [])
    |> List.map ~f:(fun p -> { p with quantity = p.quantity * nights })
  and deposit = Option.value ~default:Monetary.zero b.deposit_got
  and closing =
    "Wir danken für Ihren Besuch und freuen uns auf Ihren nächsten Aufenthalt."
  in
  { recipient; title; id; date; positions; deposit; intro; closing }
;;
