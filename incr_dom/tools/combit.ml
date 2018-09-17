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
  | s -> Printf.sprintf "### Zusatzinfo aus Combit\n%s" s

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

let booking_of_row r : Booking.t =
  { deposit_asked = flt_nz_opt r "AGEF"
  ; deposit_got = flt_nz_opt r "AEING"
  ; note = "" (* TODO *)
  ; no_tax = false
  ; company = [] (* TODO *)
  ; rooms = [] (* TODO *)
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

let main () =
  let ch = open_in "data/combit.csv" in
  let csv = Csv.of_channel ~separator:';' ~has_header:true ch in
  Csv.Rows.iter ~f:row csv;
  Printf.printf "%d customers read\n" (Hashtbl.length db);
  Hashtbl.iter (fun _i c ->
      Format.printf "%a\n%!" Sexplib.Sexp.pp_hum (sexp_of_customer c)) db

let () = main ()
