open Core_kernel
open Ghm
open Incr_dom
open Incr_dom_widgets
open Incr.Let_syntax

let form =
  let open Form.Description in
  let unvalidated_customer =
    let open Of_record in
    build_for_record (
      Customer.Fields.make_creator
        ~title:(field string)
        ~title_letter:(field string)
        ~given:(field string)
        ~second:(field string)
        ~family:(field string)
        ~company:(field string)
        ~company_address:(field string)
        ~street:(field string)
        ~street_number:(field string)
        ~postal_code:(field string)
        ~city:(field string)
        ~country:(field string)
        ~country_code:(field string)
        ~phone:(field string)
        ~phone2:(field string)
        ~mobile:(field string)
        ~fax:(field string)
        ~fax2:(field string)
        ~mail:(field string)
        ~mail2:(field string)
        ~web:(field string)
        ~keyword:(field string)
        ~note:(field string)
        ~bookings:(field (not_editable ~default:[]))
    )
  in
  let customer_form =
    conv unvalidated_customer
      ~f:(fun t _ ~block_id:_ ->
          let errors = [] in
          if List.is_empty errors
          then (Ok t)
          else (Error errors)
        )
  in
  Form.create ~name:"customer edit form" customer_form

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

let view (model : Model.t Incr.t) ~inject : Vdom.Node.t Incr.t =
  let open Vdom in
  ignore inject;
  let%map model = model in
  let state = model.form_state in
  let
    (customer_block_id,
     (title_id,
      (title_letter_id,
       (given_id, _)))) =
    Form.State.field_ids state form
  in
  let div_with_err id children =
    let no_err_node = Node.div [] [] in
    let err_node err =
      let string =
        String.strip ~drop:(fun c -> Char.equal c '"') (Error.to_string_hum err)
      in
      Node.div [Attr.style (Css.color (`Name "red"))] [ Node.text string ]
    in
    match Form.State.error state id with
    | None -> no_err_node :: children
    | Some err -> err_node err :: children
  in
  Node.body []
    [ Node.div []
        (div_with_err customer_block_id
           [ Node.div []
               (div_with_err title_id
                  [ Node.text "Anrede"
                  ; Form.Input.text state title_id []
                  ])
           ; Node.div []
               (div_with_err title_letter_id
                  [ Node.text "Anrede Brief"
                  ; Form.Input.text state title_letter_id []
                  ])
           ; Node.div []
               (div_with_err given_id
                  [ Node.text "Vorname"
                  ; Form.Input.text state given_id []
                  ])
           ]
        )
    ; Node.create "hr" [] []
    ]

let view (customer : Customer.t option Incr.t) (model : Model.t Incr.t)
  ~inject =
  let open Vdom in
  let open Node in
  let%map c = customer
  and form = view model ~inject in
  match c with
  | None -> div []
              [ text "Dieser Kunde existiert noch nicht oder nicht mehr." ]
  | Some _c ->
    div []
      [ text "Schlüssel: "
      ; form ]

let create
    ~(inject:(Action.t -> Vdom.Event.t))
    ~(model:Model.t Incr.t)
    (c: Customer.t option Incr.t) =
  let%map model = model
  and view = view c model ~inject in
  let apply_action = apply_action model in
  Component.create ~apply_action model view
