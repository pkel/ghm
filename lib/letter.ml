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

let p_list_to_string l =
  List.map ~f:(Caml.Format.asprintf "%a" (H.pp_elt ())) l |> String.concat ~sep:"\n"
;;

let attach lst =
  H.[p [b [pcdata "Anlagen:"]; br (); pcdata (String.concat ~sep:", " lst)]]
;;

type template =
  { name : string
  ; subject : string
  ; body : Html_types.p H.elt list
  ; attachments : Html_types.p H.elt list }

let empty =
  {name = "Leere Vorlage"; subject = "Kein Betreff"; body = [p' "..."]; attachments = []}
;;

let empty_with_attachments =
  { empty with
    name = empty.name ^ " (mit Anhang)"; attachments = attach ["Anlage 1"; "Anlage 2"] }
;;

let flyer =
  { name = "Hausprospekt"
  ; subject = "Hausprospekt"
  ; body =
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
  ; attachments = attach ["Hausprospekt"; "Preisliste"] }
;;

let instanciate ?(date = "") (t : template) (c : Customer.t) =
  let recipient =
    Printf.sprintf
      "<br>%s %s<br>%s<br>%s-%s %s"
      c.name.given
      c.name.family
      c.address.street_with_num
      c.address.country_code
      c.address.postal_code
      c.address.city
  and body =
    let opn = Printf.sprintf "%s %s," c.name.letter c.name.family in
    p' opn :: (t.body @ [p' "Mit freundlichen Grüßen,"; p' "Christine Keller"])
    |> p_list_to_string
  in
  { sender = "Pension Keller, Am Vögelisberg 13, D-78479 Reichenau"
  ; recipient
  ; sidebar = date
  ; subject = t.subject
  ; body
  ; attachments = p_list_to_string t.attachments }
;;
