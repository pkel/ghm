open Core_kernel
open Ghm
open Incr_dom
open Incr_dom_widgets
open Incr.Let_syntax

let name_descr =
  let open Form.Description in
  let unvalidated =
    let open Of_record in
    build_for_record (
      Customer.Name.Fields.make_creator
        ~title:(field string)
        ~given:(field string)
        ~second:(field string)
        ~family:(field string))
  in conv unvalidated
      ~f:(fun t _ ~block_id:_ ->
          let errors = [] in
          if List.is_empty errors
          then (Ok t)
          else (Error errors)
        )

let company_descr =
  let open Form.Description in
  let unvalidated =
    let open Of_record in
    build_for_record (
      Customer.Company.Fields.make_creator
        ~name:(field string)
        ~address:(field string))
  in conv unvalidated
      ~f:(fun t _ ~block_id:_ ->
          let errors = [] in
          if List.is_empty errors
          then (Ok t)
          else (Error errors)
        )

let address_descr =
  let open Form.Description in
  let unvalidated =
    let open Of_record in
    build_for_record (
      Customer.Address.Fields.make_creator
        ~street:(field string)
        ~street_number:(field string)
        ~postal_code:(field string)
        ~city:(field string)
        ~country:(field string)
        ~country_code:(field string))
  in conv unvalidated
      ~f:(fun t _ ~block_id:_ ->
          let errors = [] in
          if List.is_empty errors
          then (Ok t)
          else (Error errors)
        )

let contact_descr =
  let open Form.Description in
  let unvalidated =
    let open Of_record in
    build_for_record (
      Customer.Contact.Fields.make_creator
        ~phone:(field string)
        ~phone2:(field string)
        ~mobile:(field string)
        ~fax:(field string)
        ~fax2:(field string)
        ~mail:(field string)
        ~mail2:(field string)
        ~web:(field string))
  in conv unvalidated
    ~f:(fun t _ ~block_id:_ ->
        let errors = [] in
        if List.is_empty errors
        then (Ok t)
        else (Error errors)
      )

let customer_descr =
  let open Form.Description in
  let unvalidated_customer =
    let open Of_record in
    build_for_record (
      Customer.Fields.make_creator
        ~name:(field name_descr)
        ~address:(field address_descr)
        ~company:(field company_descr)
        ~contact:(field contact_descr)
        ~keyword:(field string)
        ~note:(field string)
        ~bookings:(field (not_editable ~default:[]))
    )
  in
  conv unvalidated_customer
    ~f:(fun t _ ~block_id:_ ->
        let errors = [] in
        if List.is_empty errors
        then (Ok t)
        else (Error errors)
      )

let form = Form.create ~name:"customer form" customer_descr

let init = Customer.empty
let form_state = Form.State.create ~init form

module Model = struct
  type t =
    { form_state : Form.State.t
    ; bogus: unit
    } [@@deriving compare]

  let create () =
    { form_state
    ; bogus = ()
    }
end

module Action = struct
  type t =
    | Update_form_state of Form.State.t sexp_opaque
  [@@deriving sexp]
end

let apply_action (model : Model.t) (action : Action.t)
    _state ~schedule_action:_ : Model.t =
  match action with
  | Update_form_state form_state -> { model with form_state }

open Vdom

let div_with_err state id children =
  let err_node err =
    let string =
      String.strip ~drop:(fun c -> Char.equal c '"') (Error.to_string_hum err)
    in
    Node.div [Attr.style (Css.color (`Name "red"))] [ Node.text string ]
  in
  match Form.State.error state id with
  | None -> children
  | Some err -> err_node err :: children

let group = Node.div [ Attr.class_ "form-group" ]

let string_field state label id =
  group
    (div_with_err state id
       [ Node.label [] [Node.text label]
       ; Form.Input.text state id [Attr.class_ "form-control"]
       ])

let view_name state ids =
  let block, (title, (given, (_second, (family, ())))) = ids in
  (* TODO: drop second from Customer.t? *)
  Node.div []
    (div_with_err state block
       [ string_field state "Titel" title
       ; string_field state "Vorname" given
       ; string_field state "Nachname" family
       ]
    )

let view_company state ids =
  let block, (name, (address, ())) = ids in
  Node.div []
    (div_with_err state block
       [ string_field state "Firma" name
       ; string_field state "Abteilung" address
       ]
    )

let view_address state ids =
  let block, (street, (number, (postal_code, (
      city, (country, (country_code, ())))))) = ids in
  Node.div []
    (div_with_err state block
       [ string_field state "Straße" street
       ; string_field state "Hausnummer" number
       ; string_field state "Postleitzahl" postal_code
       ; string_field state "Ort" city
       ; string_field state "Land" country
       ; string_field state "Ländercode" country_code
       ]
    )

let view_contact state ids =
  let block, (phone, (phone2, (mobile, (
      fax, (fax2, (mail, (mail2, (web ,())))))))) = ids in
  Node.div []
    (div_with_err state block
       [ string_field state "Telefon" phone
       ; string_field state "Telefon" phone2
       ; string_field state "Mobil" mobile
       ; string_field state "Fax" fax
       ; string_field state "Fax" fax2
       ; string_field state "Mail" mail
       ; string_field state "Mail" mail2
       ; string_field state "Internet" web
       ]
    )

let view (model : Model.t Incr.t) ~inject : Vdom.Node.t Incr.t =
  let open Vdom in
  ignore inject;
  let%map model = model in
  let state = model.form_state in
  let
    (customer_block_id,
     (name_block,
      (company_block,
       (address_block,
        (contact_block,
         (keyword_id,
          (note_id,
           ((),())))))))) =
    Form.State.field_ids state form
  in
  Node.create "form" []
    (div_with_err state customer_block_id
       [ string_field state "Schlüsselwort" keyword_id
       ; view_name state name_block
       ; view_company state company_block
       ; view_address state address_block
       ; view_contact state contact_block
       ; string_field state "Notiz" note_id
       ]
    )

let view (customer : Customer.t option Incr.t) (model : Model.t Incr.t)
  ~inject =
  let open Vdom in
  let open Node in
  let%map c = customer
  and form = view model ~inject in
  match c with
  | None -> div []
              [ text "Dieser Kunde existiert noch nicht oder nicht mehr." ]
  | Some _c -> form

let create
    ~(inject:(Action.t -> Vdom.Event.t))
    ~(model:Model.t Incr.t)
    (c: Customer.t option Incr.t) =
  let%map model = model
  and view = view c model ~inject in
  let apply_action = apply_action model in
  Component.create ~apply_action model view
