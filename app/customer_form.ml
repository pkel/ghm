open Base
open Ghm

type t = Customer.t
type env = unit

module Model = struct
  type t =
    { init : int * Customer.t
    ; cache : Customer.t
    }
  [@@deriving compare, fields]
end

let init customer =
  let open Model in
  { init = Nonce.int (), customer; cache = customer }
;;

let eval model =
  let open Model in
  let x = model.cache in
  match String.strip x.keyword with
  | "" -> Or_error.errorf "Das Schlüsselwort darf nicht leer sein."
  | keyword -> Ok { x with keyword }
;;

module Action = struct
  type name =
    | Title of string
    | Letter of string
    | Given of string
    | Family of string
  [@@deriving sexp_of, variants]

  type address =
    | Street_with_num of string
    | Postal_code of string
    | City of string
    | Country of string
    | Country_code of string
  [@@deriving sexp_of, variants]

  type contact =
    | Phone of string
    | Phone2 of string
    | Mobile of string
    | Fax of string
    | Fax2 of string
    | Mail of string
    | Mail2 of string
    | Web of string
  [@@deriving sexp_of, variants]

  type company =
    | Company_name of string
    | Company_address of string
  [@@deriving sexp_of, variants]

  type t =
    | Name of name
    | Company of company
    | Address of address
    | Contact of contact
    | Keyword of string
    | Note of string
  [@@deriving sexp_of, variants]
end

let apply_action x =
  let open Action in
  let open Customer in
  function
  | Name action ->
    let name =
      let x = x.name in
      match action with
      | Title title -> { x with title }
      | Letter letter -> { x with letter }
      | Given given -> { x with given }
      | Family family -> { x with family }
    in
    { x with name }
  | Address action ->
    let address =
      let x = x.address in
      match action with
      | Street_with_num street_with_num -> { x with street_with_num }
      | Postal_code postal_code -> { x with postal_code }
      | City city -> { x with city }
      | Country country -> { x with country }
      | Country_code country_code -> { x with country_code }
    in
    { x with address }
  | Contact action ->
    let contact =
      let x = x.contact in
      match action with
      | Phone phone -> { x with phone }
      | Phone2 phone2 -> { x with phone2 }
      | Mobile mobile -> { x with mobile }
      | Fax fax -> { x with fax }
      | Fax2 fax2 -> { x with fax2 }
      | Mail mail -> { x with mail }
      | Mail2 mail2 -> { x with mail2 }
      | Web web -> { x with web }
    in
    { x with contact }
  | Company action ->
    let company =
      let x = x.company in
      match action with
      | Company_name name -> { x with name }
      | Company_address address -> { x with address }
    in
    { x with company }
  | Note note -> { x with note }
  | Keyword keyword -> { x with keyword }
;;

let apply_action model action _state ~schedule_action:_ =
  Model.{ model with cache = apply_action model.cache action }
;;

open Action
open Bs.Form
open Incr_dom
open Incr_dom_widgets.Interactive
open Incr.Let_syntax

let input ?validator ?init label = input ?validator ?init ~label ()

let name ~inject ~(init : Customer.Name.t) =
  let x = init in
  let inject a = inject (Name a) in
  let%map title = render (input ~init:x.title "Titel") ~inject ~on_input:title
  and letter = render (input ~init:x.letter "Anrede Brief") ~inject ~on_input:letter
  and given = render (input ~init:x.given "Vorname") ~inject ~on_input:given
  and family = render (input ~init:x.family "Nachname") ~inject ~on_input:family in
  Bs.Grid.
    [ frow [ col4 [ title ]; col8 [ letter ] ]; frow [ col [ given ]; col [ family ] ] ]
;;

let company ~inject ~(init : Customer.Company.t) =
  let x = init in
  let inject a = inject (Company a) in
  let%map name = render (input ~init:x.name "Firma") ~inject ~on_input:company_name
  and address =
    render (input ~init:x.address "Abteilung") ~inject ~on_input:company_address
  in
  Bs.Grid.[ frow [ col [ name ] ]; frow [ col [ address ] ] ]
;;

let address ~inject ~(init : Customer.Address.t) =
  let x = init in
  let inject a = inject (Address a) in
  let%map street_with_num =
    render
      (input ~init:x.street_with_num "Straße und Hausnummer")
      ~inject
      ~on_input:street_with_num
  and postal_code =
    render (input ~init:x.postal_code "Postleitzahl") ~inject ~on_input:postal_code
  and city = render (input ~init:x.city "Ort") ~inject ~on_input:city
  and country = render (input ~init:x.country "Land") ~inject ~on_input:country
  and country_code =
    render (input ~init:x.country_code "Code") ~inject ~on_input:country_code
  in
  Bs.Grid.
    [ frow [ col [ street_with_num ] ]
    ; frow [ col4 [ postal_code ]; col8 [ city ] ]
    ; frow [ col8 [ country ]; col4 [ country_code ] ]
    ]
;;

let contact ~inject ~(init : Customer.Contact.t) =
  let x = init in
  let inject a = inject (Contact a) in
  let%map phone = render (input ~init:x.phone "Telefon") ~inject ~on_input:phone
  and phone2 = render (input ~init:x.phone2 "Telefon") ~inject ~on_input:phone2
  and mobile = render (input ~init:x.mobile "Mobile") ~inject ~on_input:mobile
  and fax = render (input ~init:x.fax "Fax") ~inject ~on_input:fax
  and fax2 = render (input ~init:x.fax2 "Fax") ~inject ~on_input:fax2
  and mail = render (input ~init:x.mail "Mail") ~inject ~on_input:mail
  and mail2 = render (input ~init:x.mail2 "Mail") ~inject ~on_input:mail2
  and web = render (input ~init:x.web "Internet") ~inject ~on_input:web in
  Bs.Grid.
    [ frow [ col [ phone ]; col [ phone2 ] ]
    ; frow [ col [ mobile ] ]
    ; frow [ col [ fax ]; col [ fax2 ] ]
    ; frow [ col [ mail ] ]
    ; frow [ col [ mail2 ] ]
    ; frow [ col [ web ] ]
    ]
;;

let customer ~inject ~(init : Customer.t) =
  let x = init in
  let%map name = name ~inject ~init:x.name
  and company = company ~inject ~init:x.company
  and address = address ~inject ~init:x.address
  and contact = contact ~inject ~init:x.contact
  and keyword =
    let validator value =
      match String.strip value with
      | "" -> Some "Das Schlüsselwort darf nicht leer sein!"
      | _ -> None
    in
    render (input ~validator ~init:x.keyword "Schlüsselwort") ~inject ~on_input:keyword
  and note = render (textarea ~init:x.note ~nrows:8 "Notiz") ~inject ~on_input:note in
  let left = Bs.Grid.((frow [ col [ keyword ] ] :: name) @ [ frow [ col [ note ] ] ])
  and middle = address @ company
  and right = contact in
  Bs.Grid.(frow [ col left; col middle; col right ])
;;

let create ~env:() ~(inject : Action.t -> Vdom.Event.t) (model : Model.t Incr.t) =
  let%map form =
    let%bind init = model >>| Model.init in
    let init = snd init in
    customer ~inject ~init
  and model = model in
  let apply_action = apply_action model in
  Component.create ~apply_action model form
;;
