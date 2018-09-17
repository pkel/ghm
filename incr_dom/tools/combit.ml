open Ghm
type customer = Customer.t * Booking.t list
type db = (int, customer) Hashtbl.t
let db : db = Hashtbl.create 15000

let str r f = Csv.Row.find r f |> String.trim

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

let row r =
  let bids = Csv.Row.find r "RECORDID" in
  let cids = Csv.Row.find r "GROUPID" in
  let bid = int_of_string bids in
  let cid = match int_of_string_opt cids with
    | None -> bid
    | Some cid -> cid
  in
  Hashtbl.replace db cid (customer_of_row r, [])

let main () =
  let csv = Csv.Rows.load ~separator:';' ~has_header:true "data/combit.csv" in
  List.iter row csv;
  Printf.printf "%d customers read\n" (Hashtbl.length db);
  Hashtbl.iter (fun _i (c,_) ->
      Customer.sexp_of_t c
      |> Sexplib.Sexp.pp_mach Format.std_formatter) db

let () = main ()
