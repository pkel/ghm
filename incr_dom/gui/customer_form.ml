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

module Model = struct
  type t =
    { form_state : Form.State.t
    } [@@deriving compare]

  let empty () =
    { form_state = Form.State.create form }

  let load customer =
    { form_state = Form.State.create ~init:customer form }
end

module Action = struct
  type t =
    | Update_form_state of Form.State.t sexp_opaque
  [@@deriving sexp]
end

let apply_action (_model : Model.t) (action : Action.t)
    _state ~schedule_action:_ : Model.t =
  match action with
  | Update_form_state form_state -> { form_state }

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

let field ?(input=Form.Input.text) state label id =
  group
    (div_with_err state id
       [ Node.label [] [Node.text label]
       ; input state id [Attr.class_ "form-control"]
       ])

let view_name state ids =
  let block, (title, (given, (_second, (family, ())))) = ids in
  (* TODO: drop second from Customer.t? *)
  Node.div []
    (div_with_err state block
       [ field state "Titel" title
       ; field state "Vorname" given
       ; field state "Nachname" family
       ]
    )

let view_company state ids =
  let block, (name, (address, ())) = ids in
  Node.div []
    (div_with_err state block
       [ field state "Firma" name
       ; field state "Abteilung" address
       ]
    )

let view_address state ids =
  let block, (street, (number, (postal_code, (
      city, (country, (country_code, ())))))) = ids in
  Node.div []
    (div_with_err state block
       [ field state "Straße" street
       ; field state "Hausnummer" number
       ; field state "Postleitzahl" postal_code
       ; field state "Ort" city
       ; field state "Land" country
       ; field state "Ländercode" country_code
       ]
    )

let view_contact state ids =
  let block, (phone, (phone2, (mobile, (
      fax, (fax2, (mail, (mail2, (web ,())))))))) = ids in
  Node.div []
    (div_with_err state block
       [ field state "Telefon" phone
       ; field state "Telefon" phone2
       ; field state "Mobil" mobile
       ; field state "Fax" fax
       ; field state "Fax" fax2
       ; field state "Mail" mail
       ; field state "Mail" mail2
       ; field state "Internet" web
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
       [ field state "Schlüsselwort" keyword_id
       ; view_name state name_block
       ; view_company state company_block
       ; view_address state address_block
       ; view_contact state contact_block
       ; field ~input:Form.Input.textarea state "Notiz" note_id
       ]
    )

let create
    ~(inject:(Action.t -> Vdom.Event.t))
    (model:Model.t Incr.t) =
  let%map model = model
  and view = view model ~inject in
  let apply_action = apply_action model in
  Component.create ~apply_action model view
