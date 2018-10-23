open Ghm
open Core_kernel
module String = Caml.String

let str r f = Csv.Row.find r f |> String.trim

let flt_opt r f =
  match str r f with
  | "" -> None
  | s -> Str.replace_first (Str.regexp ",") "." s |> float_of_string |> fun x -> Some x
;;

let flt_nz_opt r k =
  match flt_opt r k with Some 0. -> None | Some x -> Some x | None -> None
;;

let dat_opt r k =
  match str r k with
  | "" -> None
  | s ->
    (match String.split_on_char '.' s with
    | [d; m; y] ->
      let s = Printf.sprintf "%s-%s-%s" y m d in
      Some (Date.of_string s)
    | _ -> raise (Invalid_argument s))
;;

let coalesc prefer fallback = match prefer with None -> fallback | _ -> prefer
let coalesc_str prefer fallback = match prefer with "" -> fallback | _ -> prefer

let period_of_row r : Period.t option =
  let from = coalesc (dat_opt r "AVON") (dat_opt r "ANREISE")
  and till = coalesc (dat_opt r "ABIS") (dat_opt r "ABREISE") in
  (match from, till with
  | None, None -> None
  | Some a, Some b -> Some (a, b)
  | Some a, None -> Some (a, Date.add_days a 1)
  | None, Some b -> Some (Date.add_days b (-1), b))
  |> Option.map ~f:(function from, till -> Period.of_dates from till)
;;

let customer_note_of_row r : string =
  let f key prefix acc =
    match str r key with "" -> acc | s -> sprintf "%s%s: %s\n" acc prefix s
  in
  let id = coalesc_str (str r "GROUPID") (str r "RECORDID") in
  sprintf "Importiert aus Combit (%s).\n" id
  |> f "KUNDENNUMMER" "Kunden-Nr."
  |> f "KATEGORIE" "Kategorie"
  |> f "PLZP" "Plzp"
  |> f "POSTFACH" "Fach"
;;

let customer_of_row r : Customer.t =
  let f = str r in
  { name =
      {title = f "ANREDE"; letter = f "ANREDEBR"; given = f "VORNAME"; family = f "NAME"}
  ; company = {name = f "FIRMA"; address = f "ABTEILUNG"}
  ; address =
      { street_with_num = String.trim (f "STRASSE" ^ " " ^ f "HNR")
      ; city = f "ORT"
      ; postal_code = f "PLZZ"
      ; country = f "LAND___ausgeschrieben"
      ; country_code = f "LAND" }
  ; contact =
      { phone = f "TELEFON"
      ; phone2 = f "TELEFON2"
      ; mobile = f "MOBILTEL"
      ; fax = f "TELEFAX"
      ; fax2 = f "TELEFAX2"
      ; mail = f "EMAIL"
      ; mail2 = f "EMAIL2"
      ; web = f "INTERNET" }
  ; keyword = f "SUCH"
  ; note = customer_note_of_row r
  ; bookings = [] }
;;

let booking_note_of_row r : string =
  let posten =
    let f anzahl art preis proz acc =
      let art = str r art
      and preis = str r preis
      and anzahl = str r anzahl
      and proz = str r proz in
      match art, preis with
      | "", "0,00" -> acc
      | _ -> Printf.sprintf "%s- %sx %s à %s€ (%s%%)\n" acc anzahl art preis proz
    in
    sprintf "Importiert aus Combit (%s).\n\n" (str r "RECORDID")
    |> f "ANZAHL1" "ART1" "PREIS1" "PROZ1"
    |> f "ANZAHL2" "ART2" "PREIS2" "PROZ2"
    |> f "ANZAHL3" "ART3" "PREIS3" "PROZ3"
    |> f "ANZAHL4" "ART4" "PREIS4" "PROZ4"
  and bemerkung = str r "BEMAUFE" in
  String.trim (sprintf "%s\n%s" posten bemerkung)
;;

let guests_of_row r : Booking.guest list =
  let f given family born acc : Booking.guest list =
    let given = str r given
    and family = str r family
    and born = dat_opt r born in
    match given, family, born with
    | "", "", None -> acc
    | _ -> {given; family; born} :: acc
  in
  f "VORNAME" "NAME" "GEB_DAT01" []
  |> f "P_VORNAME" "P_NAME" "GEB_DAT02"
  |> f "P_VORNAME3" "P_NAME3" "GEB_DAT03"
  |> f "P_VORNAME4" "P_NAME4" "GEB_DAT04"
  |> f "P_VORNAME5" "P_NAME5" "GEB_DAT05"
  |> f "P_VORNAME6" "P_NAME6" "GEB_DAT06"
;;

let room_of_row_opt r room i : Booking.room option =
  let price = flt_opt r (sprintf "PREIS%i" i)
  and description = str r (sprintf "ART%i" i)
  and percent =
    Option.value ~default:100 (int_of_string_opt (str r (sprintf "PROZ%i" i)))
  in
  if description = "Kurtaxe"
  then None
  else
    match price with
    | Some price_per_bed when price_per_bed <> 0. ->
      Some
        { room
        ; beds = percent / 100
        ; price_per_bed
        ; factor = float_of_int percent /. 100.
        ; description }
    | _ -> None
;;

let rooms_of_row r : Booking.room list =
  let room = str r "ZIMMER" in
  List.filter_opt (List.map [1; 2; 3; 4] ~f:(fun i -> room_of_row_opt r room i))
;;

let booking_of_row r : Booking.t option =
  match period_of_row r with
  | None -> None
  | Some period ->
    Some
      { deposit_asked = flt_nz_opt r "AGEF"
      ; deposit_got = flt_nz_opt r "AEING"
      ; note = booking_note_of_row r
      ; guests = guests_of_row r
      ; period
      ; rooms = rooms_of_row r }
;;

let row db r =
  let bids = str r "RECORDID" in
  let cids = str r "GROUPID" in
  let bid = int_of_string bids in
  let cid = match int_of_string_opt cids with None -> bid | Some cid -> cid in
  let bookings =
    let open Customer in
    match Int.Map.find db cid with Some {bookings; _} -> bookings | None -> []
  in
  let bookings =
    match booking_of_row r with Some b -> b :: bookings | None -> bookings
  in
  let c = customer_of_row r in
  (* TODO: This assumes that best customer data is at the end of the table *)
  Int.Map.set db ~key:cid ~data:{c with bookings}
;;

include struct
  [@@@warning "-39"]

  type wrapped = {data : Customer.t}

  and post = wrapped list [@@deriving yojson]
end

let main () =
  (* Read *)
  let ch =
    if Array.length Sys.argv > 1
    then In_channel.create Sys.argv.(1)
    else In_channel.stdin
  in
  Printf.eprintf "reading...%!";
  let db =
    Csv.of_channel ~separator:';' ~has_header:true ch
    |> Csv.Rows.fold_left ~f:row ~init:Int.Map.empty
  in
  (* Write *)
  Printf.eprintf "\r%d customers read.\n%!" (Int.Map.length db);
  let l =
    Core_kernel.Int.Map.fold_right db ~f:(fun ~key:_ ~data acc -> {data} :: acc) ~init:[]
  in
  let y = Caml.([%to_yojson: wrapped list]) l in
  Yojson.Safe.to_channel stdout y
;;

let () = main ()
