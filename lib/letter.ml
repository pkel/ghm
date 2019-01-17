open Base

type t =
  { sender : string
  ; recipient : string
  ; sidebar : string
  ; subject : string
  ; body : string
  ; attachments : string }
[@@deriving yojson]

let to_b64 t = to_yojson t |> Yojson.Safe.to_string |> B64.encode

module H = Tyxml.Html

let p' s = H.(p [pcdata s])

let elts_to_string l =
  List.map ~f:(Caml.Format.asprintf "%a" (H.pp_elt ())) l |> String.concat ~sep:"\n"
;;

let generic ~subject ~body ~attachments ~sender ~signer ~date (c : Customer.t) =
  let open Printf in
  { recipient =
      H.
        [ br ()
        ; pcdata (sprintf "%s %s" c.name.given c.name.family)
        ; br ()
        ; pcdata c.address.street_with_num
        ; br ()
        ; pcdata
            (sprintf
               "%s-%s %s"
               c.address.country_code
               c.address.postal_code
               c.address.city) ]
      |> elts_to_string
  ; body =
      List.concat
        [ [p' (sprintf "%s %s," c.name.letter c.name.family)]
        ; body
        ; [p' "Mit freundlichen Grüßen,"; p' signer] ]
      |> elts_to_string
  ; sidebar = [p' date] |> elts_to_string
  ; subject
  ; attachments =
      (if List.is_empty attachments
      then ""
      else
        H.
          [p [b [pcdata "Anlagen:"]; br (); pcdata (String.concat ~sep:", " attachments)]]
        |> elts_to_string)
  ; sender }
;;

let blank = generic ~body:[p' "..."] ~subject:"Betreff"

let flyer =
  generic
    ~subject:"Hausprospekt"
    ~attachments:["Hausprospekt"; "Preisliste"]
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

let confirm ~(booking : Booking.t) =
  let open Printf in
  generic
    ~subject:"Anzahlung"
    ~attachments:[]
    ~body:
      [ p'
          (sprintf
             {|
vielen Dank für Ihre Bestellung. Hiermit bestätigen wir Ihre
Zimmerreservierung über 1 Doppelzimmer für den Zeitraum vom 19.10.2018
bis 21.10.2018. Die Übernachtung im Doppelzimmer kostet € 52.00 pro
Person und Nacht, inkl. Frühstücksbüffet. |})
      ; p'
          (match booking.deposit_asked with
          | None ->
            {|
In Ihrem Fall verzichten wir auf eine Anzahlung, möchten Sie jedoch
bitten, uns rechtzeitig abzusagen, falls bei Ihnen etwas dazwischen
kommen sollte, damit wir das Zimmer noch anderweitig vermieten können.
Danke schön! |}
          | Some deposit ->
            sprintf
              {|
Bitte überweisen Sie eine Anzahlung in Höhe von %.2f € auf unser Konto
IBAN DE74 6905 1410 0007 0356 86 BIC: SOLADES1REN. Wir erbeten eine Zahlung
innerhalb von 14 Tagen. Nach Eingang Ihrer Anzahlung gilt Ihre
Reservierung als endgültig bestätigt. |}
              deposit)
      ; p'
          {|
Es wäre schön, wenn Sie uns kurz vor Ihrem Aufenthalt Ihre ungefähre Ankunftszeit
mitteilen könnten. Danke schön! |}
      ]
;;
