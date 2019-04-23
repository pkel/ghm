(** Old combit is using clip board to transport data into excel bill. The
    following format is specified.

    2014_EXCEL_BR_V2_pQ06eaXd<¶><ZIMMER><¶><ANREDE><¶><ANREDEBR><¶><NAME><¶><VORNAME>
    <P_NAME><¶><P_VORNAME><¶><LAND><¶><PLZZ><¶><ORT><¶><STRASSE><¶><TELEFON><¶><AVON><¶><ABIS><¶><ANZAHL1><¶><ART1><¶><PREIS1><¶><PROZ1>
    <ANZAHL2><¶><ART2><¶><PREIS2><¶><PROZ2>
    <ANZAHL3><¶><ART3><¶><PREIS3>
    <ANZAHL4><¶><ART4><¶><PREIS4>
    <AEING>

    <¶> produces a new line. Thus every value is on a separate line. The output
    might look like this.

    ### BEGIN
    2014_EXCEL_BR_V2_pQ06eaXd
    11
    Herrn
    Sehr geehrter Herr
    Mustermann
    Max


    D
    78479
    Reichenau
    Am Vögelisberg 13
    040/021310231
    05.10.2015
    13.10.2015
    1
    Einzelzimmer
    50.00
    100
    0

    0.00
    100
    0

    0.00
    0

    0.00
    0.00
    ### END

    We attempt to reproduce this format for transitioning purposes. *)

open Base
open Ghm

let uniq_c_rev ~compare = function
  | [] -> []
  | hd :: tl ->
    let init = hd, 1, []
    and f (prev, cnt, acc) el =
      if compare prev el = 0 then el, cnt + 1, acc else el, 1, (cnt, prev) :: acc
    in
    let last, cnt, acc = List.fold ~f ~init tl in
    (cnt, last) :: acc
;;

let of_customer_and_booking (c : Customer.t) (b : Booking.t) =
  let from = Localize.date (Period.from b.period)
  and till = Localize.date (Period.till b.period) in
  let map_nth l i f =
    match List.nth l i with
    | Some r -> f r
    | None -> ""
  in
  let s = Booking.summarize b in
  let items =
    let compare = Booking.compare_alloc in
    List.map b.allocs ~f:(fun r -> { r with Booking.room = "" })
    |> List.sort ~compare
    |> uniq_c_rev ~compare
  in
  let mon_to_string = Monetary.to_string_dot in
  let lines =
    let open Booking in
    let comp = Fn.compose in
    let mult a b = a * b in
    [ "2014_EXCEL_BR_V2_pQ06eaXd"
    ; map_nth b.allocs 0 room
    ; c.name.title
    ; c.name.letter
    ; c.name.family
    ; c.name.given
    ; "" (* Partner Name, nicht auf Rechnung *)
    ; "" (* Partner Vorname, nicht auf Rechnung *)
    ; c.address.country_code
    ; c.address.postal_code
    ; c.address.city
    ; c.address.street_with_num
    ; c.contact.phone
    ; from
    ; till
    ; map_nth items 0 (comp Int.to_string fst)
    ; map_nth items 0 (comp description snd)
    ; map_nth items 0 (comp mon_to_string (comp price_per_bed snd))
    ; map_nth items 0 (comp Int.to_string (comp (mult 100) (comp beds snd)))
    ; map_nth items 1 (comp Int.to_string fst)
    ; map_nth items 1 (comp description snd)
    ; map_nth items 1 (comp mon_to_string (comp price_per_bed snd))
    ; map_nth items 1 (comp Int.to_string (comp (mult 100) (comp beds snd)))
    ; map_nth items 2 (comp Int.to_string fst)
    ; map_nth items 2 (comp description snd)
    ; map_nth items 2 (comp mon_to_string (comp price_per_bed snd))
    ; (if b.tax_free then "0" else Int.to_string s.tax_payers)
    ; (if s.tax_payers < 1 || b.tax_free then "" else "Kurtaxe")
    ; (if s.tax_payers < 1 || b.tax_free then "0.00" else "2.00")
    ; mon_to_string (Option.value ~default:Monetary.zero b.deposit_got)
    ]
  in
  String.concat ~sep:"\n" lines
;;
