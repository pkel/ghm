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

(* TODO: move local tax pricing from invoice / invoice_gen to here *)
let xml (c : Customer.t) (b : Booking.t) =
  let open Booking in
  let open XMLgen in
  let base = Period.till b.period in
  let categorize g =
    let younger x =
      match g.born with
      | Some birthdate -> Date.compare (Date.add_years base (-x)) birthdate < 0
      | None -> false
    in
    if younger 6 then "7" else if younger 15 then "8" else "1"
    (* unfortunately we do not know the key for the birth date
     * it is not one of geburtsdatum, geburtdatum, geburt, geboren-am, geboren,
     * and geburtstag
  and born g =
    match g.born with
    | Some birthdate -> d "geburt" (Date.to_string birthdate)
    | None -> n "geburtstag-unbekannt" []
     *)
  in
  xml
    (n
       "meldescheine"
       [ n
           "meldeschein"
           ([ d "hotelid" "0550"
            ; d "anreise" (Period.from b.period |> Date.to_string)
            ; d "abreise" (Period.till b.period |> Date.to_string)
            ; d "strasse" c.address.street_with_num
            ; d "hausnummer" "."
            ; d "plz" c.address.postal_code
            ; d "ort" c.address.city
            ; d "land" c.address.country
            ]
           @
           match b.guests with
           | [] -> [ d "kategorie" "1"; d "name" c.name.family; d "vorname" c.name.given ]
           | hd :: tl ->
             [ d "kategorie" (categorize hd)
             ; d "name" hd.family
             ; d "vorname" hd.given
               (* ; born hd *)
             ]
             @ List.map tl ~f:(fun g ->
                   n
                     "begleitperson"
                     [ d "kategorie" (categorize g)
                     ; d "name" g.family
                     ; d "vorname" g.given
                       (* ; born g *)
                     ]))
       ])
;;
