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

let nonempty_string errmsg =
  let open Form.Description in
  conv_without_block string ~f:(fun s id -> match s with
      | "" -> Error [Form.Form_error.create ~id (Error.of_string errmsg)]
      | s -> Ok s)

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
        ~keyword:(field (nonempty_string "Schlüsselwort darf nicht leer sein"))
        ~note:(field string)
        ~bookings:(field (not_editable ~default:[]))
    )
  in
  conv unvalidated_customer ~f:(fun t _ ~block_id:_ -> Ok t)

let form = Form.create ~name:"customer form" customer_descr

module Model = struct
  type t =
    { form_state : Form.State.t
    } [@@deriving compare, fields]

  let empty () =
    { form_state = Form.State.create form }

  let load customer =
    { form_state = Form.State.create ~init:customer form }
end

module Action = struct
  type t =
    | Update of Form.State.t sexp_opaque
  [@@deriving sexp]
end

module State = struct type t = unit end

let apply_action (model: Model.t) (action: Action.t) (_state: State.t)
    ~schedule_action:_ : Model.t =
  ignore model;
  match action with
  | Update form_state -> Log.form form_state; { form_state }

open Vdom

let div_with_err state id children =
  (* TODO: error on field should be handled separately. *)
  let err_node err =
    let msg =
      String.strip ~drop:(fun c -> Char.equal c '"') (Error.to_string_hum err)
    in
    Node.div [Attr.classes ["alert"; "alert-danger"]] [ Node.text msg ]
  in
  match Form.State.error state id with
  | None -> children
  | Some err -> children @ [err_node err]

let group = Node.div [ Attr.class_ "form-group" ]

let field ?(input=Form.Input.text) state label id =
  group
    (div_with_err state id
       [ Node.label [] [Node.text label]
       ; input state id [Attr.class_ "form-control"]
       ])

let view_name state ids =
  let block, (title, (given, (second, (family, ())))) = ids in
  (* TODO: drop second from Customer.t? *)
  Node.div []
    (div_with_err state block
       [ field state "Titel" title
       ; field state "Vorname" given
       ; field state "Weitere Vornamen" second
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

let view (model : Model.t Incr.t) ~inject ~save : Vdom.Node.t Incr.t =
  let open Vdom in
  let%map state = model >>| Model.form_state in
  let
    (block_id,
     (name_block,
      (company_block,
       (address_block,
        (contact_block,
         (keyword_id,
          (note_id,
           ((),())))))))) =
    Form.State.field_ids state form
  in
  let save _evt =
    let c_opt, new_state = Form.State.read_value state form in
    match c_opt with
    | None -> inject (Action.Update new_state)
    | Some c -> save c
  in
  Node.create "form" [] [
    Node.div []
      (div_with_err state block_id
         [ Bs.button save "Speichern"
         ; field state "Schlüsselwort" keyword_id
         ; view_name state name_block
         ; view_company state company_block
         ; view_address state address_block
         ; view_contact state contact_block
         ; field ~input:Form.Input.textarea state "Notiz" note_id
         ]
      )]

let create
    ~(save: Customer.t -> Vdom.Event.t)
    ~(inject: Action.t -> Vdom.Event.t)
    (model:Model.t Incr.t) =
  let%map model = model
  and view = view ~inject ~save model in
  let apply_action = apply_action model in
  Component.create ~apply_action model view
