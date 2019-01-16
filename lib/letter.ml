open Base

type t =
  { sender : string
  ; recipient : string
  ; sidebar : string
  ; subject : string
  ; body : string
  ; attachments : string }
[@@deriving yojson]

type template =
  { name : string
  ; subject : string
  ; body : string
  ; attachments : string }
[@@deriving yojson, compare, sexp_of]

let base =
  { name = "Leere Vorlage"
  ; subject = "Kein Betreff"
  ; body = "<p>...</p>"
  ; attachments = "" }
;;

let base_with_attachments =
  { base with
    name = base.name ^ " (mit Anhang)"
  ; attachments = "<p><b>Anlagen:</b><br>Anlage 1, Anlage 2</p>" }
;;

let to_b64 t = to_yojson t |> Yojson.Safe.to_string |> B64.encode

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
    Printf.sprintf
      "<p>%s %s,</p>%s<p>Mit freundlichen Grüßen</p><p></p><p>Christine Keller</p>"
      c.name.letter
      c.name.family
      t.body
  in
  { sender = "Pension Keller, Am Vögelisberg 13, D-78479 Reichenau"
  ; recipient
  ; sidebar = date
  ; subject = t.subject
  ; body
  ; attachments = t.attachments }
;;
