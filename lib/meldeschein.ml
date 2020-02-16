open Core_kernel

module XMLgen : sig
  type attr
  type node

  val a : string -> string -> attr
  val n : string -> ?a:attr list -> node list -> node
  val d : string -> ?a:attr list -> string -> node
  val xml : node -> string
end = struct
  type attr = string
  type node = string

  let a key value : attr = sprintf "%s=\"%s\"" key value

  let n key ?(a = []) : node list -> node =
    let attr =
      match a with
      | [] -> ""
      | l -> sprintf " %s" (String.concat ~sep:" " l)
    in
    function
    | [] -> sprintf "<%s%s />" key attr
    | children ->
      let value = String.concat children in
      sprintf "<%s%s>%s</%s>" key attr value key
  ;;

  let d key ?a = function
    | "" -> n key ?a []
    | s -> n key ?a [ s ]
  ;;

  let xml node = sprintf "<?xml version=\"1.0\" encoding=\"utf-8\"?>%s" node
end

let gen (c : Customer.t) (b : Booking.t) =
  let open XMLgen in
  xml
    (n
       "meldescheine"
       [ n
           "meldeschein"
           [ d "Herkunfst-ID" "Pension Keller / ghm"
           ; d "Benutzer" "Import"
           ; d "hotelid" "0020"
           ; d "anrede" c.name.title
           ; d "name" c.name.family
           ; d "vorname" c.name.given
           ; d "strasse" c.address.street_with_num
           ; n "hausnummer" []
           ; d "plz" c.address.postal_code
           ; d "ort" c.address.city
           ; d "land" c.address.country
           ; d "anreise" (Period.from b.period |> Date.to_string)
           ; d "abreise" (Period.till b.period |> Date.to_string)
           ]
       ])
;;
