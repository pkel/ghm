open Ghm
open Sexplib.Std

type customer = Customer.t * Booking.t list [@@deriving sexp]
type db = (int, customer) Hashtbl.t
let db : db = Hashtbl.create 15000

let str r f = Csv.Row.find r f |> String.trim
let flt_opt r f =
  match str r f with
  | "" -> None
  | s -> begin Str.replace_first (Str.regexp ",") "." s
               |> float_of_string
               |> fun x -> Some x end
let flt_nz_opt r k = match flt_opt r k with
  | Some 0. -> None
  | Some x -> Some x
  | None -> None

let dat_opt r k = match str r k with
  | "" -> None
  | s -> match String.split_on_char '.' s with
      | [d;m;y] -> let s = Printf.sprintf "%s-%s-%s" y m d in
        Some (Core_kernel.Date.of_string s)
      | _ -> raise (Invalid_argument s)

let customer_note_of_row r : string =
  let f key prefix acc =
    match str r key with
    | "" -> acc
    | s -> Printf.sprintf "%s%s: %s\n" acc prefix s
  in
  f "KUNDENNUMMER" "Kunden-Nr." ""
  |> f "KATEGORIE" "Kategorie"
  |> f "PLZP" "Plzp"
  |> f "POSTFACH" "Fach"
  |> function
  | "" -> ""
  | s -> Printf.sprintf "### Zusatzinfo aus Combit\n\n%s" s

let customer_of_row r : Customer.t =
  let f = str r in
  { title           = f "ANREDE"
  ; title_letter    = f "ANREDEBR"
  ; given           = f "VORNAME"
  ; second          = ""
  ; family          = f "NAME"
  ; company         = f "FIRMA"
  ; company_address = f "ABTEILUNG"
  ; keyword         = f "SUCH"
  ; street          = f "STRASSE"
  (* TODO: Read street number from street if absent *)
  ; street_number   = f "HNR"
  ; city            = f "ORT"
  ; postal_code     = f "PLZZ"
  ; country         = f "LAND___ausgeschrieben"
  ; country_code    = f "LAND"
  ; phone           = f "TELEFON"
  ; phone2          = f "TELEFON2"
  ; mobile          = f "MOBILTEL"
  ; fax             = f "TELEFAX"
  ; fax2            = f "TELEFAX2"
  ; mail            = f "EMAIL"
  ; mail2           = f "EMAIL2"
  ; web             = f "INTERNET"
  ; note            = customer_note_of_row r
  }

let booking_note_of_row r : string =
  let posten =
    let f anzahl art preis proz acc =
      let art = str r art
      and preis = str r preis
      and anzahl = str r anzahl
      and proz = str r proz
      in match art, preis with
      | "", "0,00" -> acc
      | _ -> Printf.sprintf "%s  - %sx %s à %s€ (%s%%)\n"
               acc anzahl art preis proz
    in
    f "ANZAHL1" "ART1" "PREIS1" "PROZ1" ""
    |> f "ANZAHL2" "ART2" "PREIS2" "PROZ2"
    |> f "ANZAHL3" "ART3" "PREIS3" "PROZ3"
    |> f "ANZAHL4" "ART4" "PREIS4" "PROZ4"
    |> function
    | "" -> ""
    | s -> Printf.sprintf "### Posten aus Combit\n\n%s\n" s
  and bemerkung = match str r "BEMAUFE" with
    | "" -> ""
    | s -> Printf.sprintf "### Bemerkung aus Combit\n\n%s" s
  in posten ^ bemerkung

let guests_of_row r : Booking.guest list =
  let f given family born acc : Booking.guest list =
    let given = str r given
    and family = str r family
    and born = dat_opt r born
    in match given, family, born with
    | "","",None -> acc
    | _ -> { given; second=""; family; born} :: acc
  in
  f "VORNAME" "NAME" "GEB_DAT01" []
  |> f "P_VORNAME"  "P_NAME"  "GEB_DAT02"
  |> f "P_VORNAME3" "P_NAME3" "GEB_DAT03"
  |> f "P_VORNAME4" "P_NAME4" "GEB_DAT04"
  |> f "P_VORNAME5" "P_NAME5" "GEB_DAT05"
  |> f "P_VORNAME6" "P_NAME6" "GEB_DAT06"

let rooms_of_row _r : Booking.room list = []

let booking_of_row r : Booking.t =
  { deposit_asked = flt_nz_opt r "AGEF"
  ; deposit_got = flt_nz_opt r "AEING"
  ; note = booking_note_of_row r
  ; no_tax = false
  ; guests = guests_of_row r
  ; rooms = rooms_of_row r
  }

let row r =
  let bids = Csv.Row.find r "RECORDID" in
  let cids = Csv.Row.find r "GROUPID" in
  let bid = int_of_string bids in
  let cid = match int_of_string_opt cids with
    | None -> bid
    | Some cid -> cid
  in
  let bl =
    match Hashtbl.find_opt db cid with
    | Some (_, bl) -> bl
    | None -> []
  in
  Hashtbl.replace db cid (customer_of_row r, booking_of_row r :: bl)

(* TODO: postgrest/import.sql does some smart coalescing in order to lift
   very old entries to latest version *)

let main () =
  (* Read *)
  let ch = if Array.length Sys.argv > 1 then open_in Sys.argv.(1) else stdin in
  Csv.of_channel ~separator:';' ~has_header:true ch |> Csv.Rows.iter ~f:row;
  (* Write *)
  Hashtbl.iter (fun _i c ->
      Format.printf "%a\n%!" Sexplib.Sexp.pp_hum (sexp_of_customer c)) db;
  (* Status *)
  Printf.eprintf "%d customers processed.\n%!" (Hashtbl.length db)

let () = main ()
