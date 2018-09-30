open Ghm
module Date = Core_kernel.Date
module Option = Core_kernel.Option

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
        Some (Date.of_string s)
      | _ -> raise (Invalid_argument s)

let coalesc prefer fallback =
  match prefer with
  | None -> fallback
  | _ -> prefer

let period_of_row r : Period.t option =
  let from = coalesc (dat_opt r "AVON") (dat_opt r "ANREISE")
  and till = coalesc (dat_opt r "ABIS") (dat_opt r "ABREISE")
  in
  ( match from, till with
    | None, None -> None
    | Some a, Some b -> Some (a, b)
    | Some a, None -> Some (a, Date.add_days a 1)
    | None, Some b -> Some (Date.add_days b (-1), b)
  ) |> Option.map ~f:(function from, till -> Period.of_dates from till)

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
  { name =
      { title  = f "ANREDE"
      (* ; title_letter = f "ANREDEBR" *)
      ; given  = f "VORNAME"
      ; second = ""
      ; family = f "NAME"
      }
  ; company         =
      { name    = f "FIRMA"
      ; address = f "ABTEILUNG"
      }
  ; address =
      (* TODO: Read street number from street if absent *)
      { street        = f "STRASSE"
      ; street_number = f "HNR"
      ; city          = f "ORT"
      ; postal_code   = f "PLZZ"
      ; country       = f "LAND___ausgeschrieben"
      ; country_code  = f "LAND"
      }
  ; contact =
      { phone  = f "TELEFON"
      ; phone2 = f "TELEFON2"
      ; mobile = f "MOBILTEL"
      ; fax    = f "TELEFAX"
      ; fax2   = f "TELEFAX2"
      ; mail   = f "EMAIL"
      ; mail2  = f "EMAIL2"
      ; web    = f "INTERNET"
      }
  ; keyword  = f "SUCH"
  ; note     = customer_note_of_row r
  ; bookings = []
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

let rooms_of_row r : Booking.room list =
  let open Booking in
  (* TODO: import more fields *)
  match period_of_row r with
  | Some period ->
    [{ period
     ; room="n/a"
     ; beds=0
     ; price_per_bed=0.
     ; factor=1.
     ; description="aus Combit"
     }]
  | None -> []

let booking_of_row r : Booking.t option =
  match rooms_of_row r with
  | [] -> None
  | rooms -> Some
    { deposit_asked = flt_nz_opt r "AGEF"
    ; deposit_got = flt_nz_opt r "AEING"
    ; note = booking_note_of_row r
    ; no_tax = false
    ; guests = guests_of_row r
    ; rooms
    }

let row db r =
  let bids = Csv.Row.find r "RECORDID" in
  let cids = Csv.Row.find r "GROUPID" in
  let bid = int_of_string bids in
  let cid = match int_of_string_opt cids with
    | None -> bid
    | Some cid -> cid
  in
  let bookings =
    let open Customer in
    match Storage.load db cid with
    | Some {bookings;_} -> bookings
    | None -> []
  in
  let bookings =
    match booking_of_row r with
    | Some b -> b :: bookings
    | None -> bookings
  in
  let c = customer_of_row r in
  Storage.save db ~key:cid ~data:{ c with bookings }

let main () =
  (* Read *)
  let ch = if Array.length Sys.argv > 1 then open_in Sys.argv.(1) else stdin in
  Printf.eprintf "reading...%!";
  let db =
    Csv.of_channel ~separator:';' ~has_header:true ch
    |> Csv.Rows.fold_left ~f:row ~init:Storage.empty
  in
  (* Write *)
  if Array.length Sys.argv > 2 then begin (* chunks into folder *)
    let d = Sys.argv.(2) in
    if not (Sys.is_directory d) then
      raise (Invalid_argument (d ^ "is not a directory"));
    let is_chunk s =
      let r = Str.regexp "^chunk-[0-9]+.sexp$" in
      Str.string_match r s 0
    in
    Printf.eprintf "\rcleaning...%!";
    Array.iter (fun f -> if is_chunk f then Sys.remove (d ^ "/" ^ f))
      (Sys.readdir d);
    let n = List.fold_left (fun i c ->
        Printf.eprintf "\rbuilding chunk %d%!" i;
        let oc = open_out (d ^ "/chunk-" ^ string_of_int i ^ ".sexp") in
        let fmt = Format.formatter_of_out_channel oc in
        Storage.pp_hum fmt c;
        close_out oc;
        i + 1
      ) 0 (Storage.chunks ~firstsize:97 ~size:587 db) in
    Printf.eprintf "\r%d customers processed into %d chunks.\n%!"
      (Storage.size db) n
  end else begin
    Storage.pp_hum Format.std_formatter db;
    Printf.eprintf "\r%d customers processed.\n%!" (Storage.size db)
  end

let () = main ()
