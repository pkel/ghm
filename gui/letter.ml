open Ghm
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

let base_date_to_string d =
  Date_yojson.to_string d |> Browser.Date.of_string |> Browser.Date.to_locale_date_string
;;

let confirm ~(booking : Booking.t) =
  let period = Period.to_string_hum base_date_to_string booking.period in
  let positions =
    let n = List.length booking.allocs in
    let comma i = match n - i with 1 -> "" | 2 -> " und" | _ -> "," in
    List.mapi
      ~f:(fun i x -> H.li [H.pcdata (Booking.string_of_alloc x ^ comma i)])
      booking.allocs
  in
  let open Printf in
  generic
    ~subject:"Anzahlung"
    ~attachments:[]
    ~body:
      [ p'
          {|
vielen Dank für Ihre Bestellung. Hiermit bestätigen wir Ihre
Zimmerreservierung über |}
      ; H.ul positions
      ; p' (sprintf {| für den Zeitraum vom %s. |} period)
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
Bitte überweisen Sie eine Anzahlung in Höhe von %.2f€ auf unten angebenes
Konto. Wir erbeten eine Zahlung innerhalb von 14 Tagen. Nach Eingang Ihrer
Anzahlung gilt Ihre Reservierung als endgültig bestätigt. |}
              deposit)
      ; p'
          {|
Es wäre schön, wenn Sie uns kurz vor Ihrem Aufenthalt Ihre ungefähre
Ankunftszeit mitteilen könnten. Danke schön! |}
      ]
;;
