open Ghm
open Base

type t =
  { sender : string
  ; recipient : string
  ; sidebar : string
  ; subject : string
  ; body : string
  ; attachments : string
  }
[@@deriving yojson]

let sender = "Pension Keller, Am Vögelisberg 13, D-78479 Reichenau"
let signer = "Christine Keller"

let to_b64 t =
  to_yojson t
  |> Yojson.Safe.to_string
  |> Base64.encode
  |> function
  | Ok str -> str (* TODO: care properly *)
  | Error _ -> ""
;;

let href t = "/letter/#" ^ to_b64 t

module H = Tyxml.Html

let p' s = H.(p [ txt s ])

let elts_to_string l =
  List.map ~f:(Caml.Format.asprintf "%a" (H.pp_elt ())) l |> String.concat ~sep:"\n"
;;

let opening (c : Customer.t) = Printf.sprintf "%s %s," c.name.letter c.name.family

let recipient (c : Customer.t) =
  let open Printf in
  H.
    [ br ()
    ; txt (sprintf "%s %s" c.name.given c.name.family)
    ; br ()
    ; txt c.address.street_with_num
    ; br ()
    ; txt
        (sprintf "%s-%s %s" c.address.country_code c.address.postal_code c.address.city)
    ]
;;

let generic ~subject ~body ~attachments ~date (c : Customer.t) =
  { recipient = recipient c |> elts_to_string
  ; body =
      List.concat
        [ [ p' (opening c) ]
        ; body
        ; [ H.br (); p' "Mit freundlichen Grüßen"; p' signer ]
        ]
      |> elts_to_string
  ; sidebar = [ p' date ] |> elts_to_string
  ; subject
  ; attachments =
      (if List.is_empty attachments
      then ""
      else
        H.
          [ p [ b [ txt "Anlagen:" ]; br (); txt (String.concat ~sep:", " attachments) ] ]
        |> elts_to_string)
  ; sender
  }
;;

let blank = generic ~body:[ p' "..." ] ~subject:"Betreff"

let flyer =
  generic
    ~subject:"Hausprospekt"
    ~attachments:[ "Hausprospekt"; "Preisliste" ]
    ~body:
      [ p'
          {|
wir freuen uns sehr über Ihr Interesse an unserem Haus und darüber dass Sie
Ihren Urlaub bei uns verbringen wollen. Anbei erhalten Sie die gewünschten
Unterlagen. |}
      ; p'
          {|
Wir hoffen, dass Ihnen unser Haus zusagt und freuen uns über Ihren Anruf oder
Ihre schriftliche Zimmerreservierung. |}
      ]
;;

let base_date_to_string d =
  Date.to_string d |> Browser.Date.of_string |> Browser.Date.to_locale_date_string
;;

let confirm ~(booking : Booking.t) =
  let period = Period.to_string_hum booking.period in
  let positions =
    let n = List.length booking.allocs in
    let comma i =
      match n - i with
      | 1 -> ""
      | 2 -> " und"
      | _ -> ","
    in
    List.mapi
      ~f:(fun i x -> H.li [ H.txt (Booking.string_of_alloc x ^ comma i) ])
      booking.allocs
  in
  let open Printf in
  generic
    ~subject:"Reservierungsbestätigung"
    ~attachments:[]
    ~body:
      [ p'
          {|
vielen Dank für Ihre Bestellung. Hiermit bestätigen wir Ihre
Zimmerreservierung über |}
      ; H.ul positions
      ; p'
          (sprintf
             {| für den Zeitraum vom %s. Unsere Preise
      beinhalten Frühstück vom Buffet.|}
             period)
      ; p'
          (match booking.deposit_asked with
          | None ->
            {|
In Ihrem Fall verzichten wir auf eine Anzahlung, möchten Sie jedoch
bitten, uns rechtzeitig abzusagen, falls bei Ihnen etwas dazwischen
kommen sollte, damit wir das Zimmer noch anderweitig vermieten können.
Danke schön! |}
          | Some deposit ->
            Caml.Format.asprintf
              {|
Bitte überweisen Sie eine Anzahlung in Höhe von %a € auf unten angegebenes
Konto. Wir erbeten eine Zahlung innerhalb von 14 Tagen. Nach Eingang Ihrer
Anzahlung gilt Ihre Reservierung als endgültig bestätigt. |}
              Monetary.print
              deposit)
      ; p'
          {|
Es wäre schön, wenn Sie uns kurz vor Ihrem Aufenthalt Ihre ungefähre
Ankunftszeit mitteilen könnten. Danke schön! |}
      ]
;;

let invoice (inv : Invoice.t) =
  let open H in
  let tdr = td ~a:[ a_style "text-align:right" ]
  and th s = th ~a:[ a_style ("text-align:" ^ s) ] in
  let open Printf in
  let open Invoice in
  let s = summary inv in
  let body =
    [ p' inv.intro
    ; table
        ~a:[ a_style "width:100%" ]
        ~thead:
          (thead
             [ tr
                 [ th "left" [ txt "Anzahl" ]
                 ; th "left" [ txt "Beschreibung" ]
                 ; th "right" [ txt "Steuersatz" ]
                 ; th "right" [ txt "Einzelpreis" ]
                 ; th "right" [ txt "Preis" ]
                 ]
             ])
        (List.map
           ~f:(fun p ->
             tr
               [ td [ txt (sprintf "%dx" p.quantity) ]
               ; td [ txt p.description ]
               ; tdr [ txt (sprintf "%i%%" p.tax) ]
               ; tdr [ txt (sprintf "%s€" (Monetary.to_string p.price)) ]
               ; tdr
                   [ txt
                       (sprintf "%s€" Monetary.(to_string (times p.quantity p.price)))
                   ]
               ])
           inv.positions
        @ [ tr
              [ td []
              ; td []
              ; td []
              ; th "right" [ txt "Summe" ]
              ; tdr [ txt (sprintf "%s€" (Monetary.to_string s.sum)) ]
              ]
          ; tr
              [ td []
              ; td []
              ; td []
              ; th "right" [ txt "Anzahlung" ]
              ; tdr [ txt (sprintf "%s€" (Monetary.to_string inv.deposit)) ]
              ]
          ; tr
              [ td []
              ; td []
              ; td []
              ; th "right" [ txt "Restsumme" ]
              ; tdr [ txt (sprintf "%s€" Monetary.(s.sum - inv.deposit |> to_string)) ]
              ]
          ])
    ; table
        ~a:[ a_style "text-align:right;font-size:0.8em;margin-bottom:2rem" ]
        ~thead:
          (thead
             [ tr
                 [ H.th
                     ~a:[ a_colspan 2; a_style "text-align:right" ]
                     [ txt "enthaltene Mehrwertsteuer" ]
                 ]
             ])
        (List.map
           ~f:(fun (rate, v) ->
             tr
               [ tdr [ txt (sprintf "%i%%" rate) ]
               ; tdr [ txt (sprintf "%s€" (Monetary.to_string v)) ]
               ])
           s.included_tax)
    ; p' inv.closing
    ]
    |> elts_to_string
  and attachments = [] |> elts_to_string in
  { sender = ""
  ; recipient = String.substr_replace_all inv.recipient ~pattern:"\n" ~with_:"<br>"
  ; sidebar =
      (let d = Option.(map ~f:Localize.date inv.date |> value ~default:"") in
       match inv.id with
       | Some id -> sprintf "%s<br>%s" id d
       | None -> d)
  ; attachments
  ; body
  ; subject = inv.title
  }
;;
