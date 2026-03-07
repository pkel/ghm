open Core_kernel
open Invoice

(* TODO: it might be nice to split the period and handle kurtaxe on a
 * daily basis. *)
let tax_unit date =
  (* https://www.reichenau-tourismus.de/de/planen-buchen/kurtaxe-gaestekarte *)
  let y = Date.year date
  and m = Date.month date |> Month.to_int in
  let offseason, season =
    if y <= 2021
    then Monetary.cents 80, Monetary.cents 200
    else if y <= 2025
    then Monetary.cents 100, Monetary.cents 250
    else Monetary.cents 150, Monetary.cents 330
  in
  if m < 4 || m > 10 then offseason else season
;;

let gen ?invoice_date (c : Customer.t) (b : Booking.t) =
  let nights = Period.nights b.period
  and s = Booking.Summary.of_booking b
  and describe (a : Booking.alloc) =
    Printf.sprintf "Übernachtung im %s" a.description
  in
  let title = "Rechnung"
  and id =
    match Booking.Rooms.first s.rooms with
    | Some room ->
      let date = Period.till b.period in
      let open Date in
      let d = day date
      and m = month date |> Month.to_int
      and y = year date % 1000 in
      let room =
        match int_of_string_opt room with
        | Some i -> sprintf "%03i" i
        | None -> room
      in
      let s = sprintf "%02i%02i%02i-%s" y m d room in
      Some s
    | _ -> None
  and departure_date = Some (Period.till b.period)
  and tax_unit =
    let date = Period.till b.period in
    tax_unit date
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
    (List.map b.allocs ~f:(fun a ->
         { quantity = a.beds
         ; description = describe a
         ; price = a.price_per_bed
         ; tax = Reduced7With3EuroDrinks19
         })
    @
    if s.tax_payers > 0 && not b.tax_free
    then
      [ { quantity = s.tax_payers
        ; price = tax_unit
        ; description = "Kurtaxe"
        ; tax = Reduced7
        }
      ]
    else [])
    |> List.map ~f:(fun p -> { p with quantity = p.quantity * nights })
  and deposit = Option.value ~default:Monetary.zero b.deposit_got
  and closing =
    "Wir hoffen, dass es Ihnen bei uns gefallen hat und freuen uns, wenn Sie uns wieder \
     einmal besuchen."
  in
  { recipient
  ; title
  ; id
  ; invoice_date
  ; departure_date
  ; positions
  ; deposit
  ; intro
  ; closing
  }
;;
