(* TODO:
   - make this an invoice option form.
   - include room number into invoice id
   - use interactive like in customer form
*)
open Interfaces
open Core_kernel
open Ghm
open Incr_dom
open Incr.Let_syntax

type env = { reload : unit inject }

module Model = struct
  open Invoice

  type t =
    { local : Invoice.t
    ; dummy : unit
    }
  [@@deriving compare, fields]

  let append_empty_pos inv = { inv with positions = inv.positions @ [ empty_position ] }

  let strip_empty_pos inv =
    { inv with positions = List.filter ~f:(fun p -> p.description <> "") inv.positions }
  ;;

  let ensure_empty_last m =
    match List.last m.local.positions with
    | Some e when e <> empty_position -> { m with local = append_empty_pos m.local }
    | None -> { m with local = append_empty_pos m.local }
    | _ -> m
  ;;

  let load inv = { local = inv; dummy = () } |> ensure_empty_last
  let read x = x.local
end

module Action = struct
  type position =
    | Quantity of string
    | Description of string
    | Price of string
    | Tax of string
  [@@deriving sexp, variants]

  type invoice =
    | Recipient of string
    | Title of string
    | Date of string
    | Id of string
    | Position of int * position
    | Deposit of string
    | Intro of string
    | Closing of string
  [@@deriving sexp, variants]

  type t =
    | Field of invoice
    | SetId
  [@@deriving sexp, variants]
end

let apply_action (model : Model.t)
                 (action : Action.t)
                 (state : State.t)
                 ~schedule_action
    : Model.t
  =
  let open Invoice in
  let _ = state, schedule_action in
  let form f = { model with local = f model.local } in
  let form_p i f =
    let f inv =
      { inv with
        positions = List.mapi ~f:(fun i' p -> if i <> i' then p else f p) inv.positions
      }
    in
    form f
  in
  let strip = String.strip in
  let model =
    match action with
    | Field action ->
      (match action with
      | Recipient s -> form (fun i -> { i with recipient = strip s })
      | Title s -> form (fun i -> { i with title = strip s })
      | Intro s -> form (fun i -> { i with intro = strip s })
      | Closing s -> form (fun i -> { i with closing = strip s })
      | Id id ->
        (match strip id with
        | "" -> form (fun i -> { i with id = None })
        | id -> form (fun i -> { i with id = Some id }))
      | Date date ->
        (match Date.of_string date with
        | date -> form (fun i -> { i with date = Some date })
        | exception _ -> model (* TODO: handle *))
      | Position (i, Description s) ->
        form_p i (fun p -> { p with description = strip s })
      | Position (i, Quantity quantity) ->
        (match int_of_string quantity with
        | quantity -> form_p i (fun p -> { p with quantity })
        | exception _ -> model (* TODO: handle *))
      | Position (i, Tax tax) ->
        (match int_of_string tax with
        | tax -> form_p i (fun p -> { p with tax })
        | exception _ -> model (* TODO: handle *))
      | Position (i, Price price) ->
        (match Monetary.of_float (float_of_string price) with
        | Some price -> form_p i (fun p -> { p with price })
        | None -> model
        | exception _ -> model (* TODO: handle *))
      | Deposit deposit ->
        (match Monetary.of_float (float_of_string deposit) with
        | Some deposit -> form (fun i -> { i with deposit })
        | None -> model
        | exception _ -> model (* TODO: handle *)))
    | SetId ->
      let local =
        let l = model.local in
        { l with id = Some (Invoice_gen.id l) }
      in
      Model.load local
  in
  Model.ensure_empty_last model
;;

open Vdom
open Vdom_form

let view (model : Model.t Incr.t) ~env ~inject =
  let id_btn =
    Bs.button ~i:(S "magic") ~action:(fun _ -> inject Action.setid) "Nummer Erfinden"
  in
  let inject = Fn.compose inject Action.field in
  let title_f = labelled_input "Titel"
  and recipient_f = labelled_textfield ~rows:4 "Empfänger"
  and intro_f = labelled_textfield ~rows:1 "Freitext"
  and closing_f = labelled_textfield ~rows:1 "Freitext"
  and date_f = labelled_input ~type_:Date "Datum"
  and id_f = labelled_input ~append:[ id_btn ] "Rechnungsnummer"
  and quantity_f = labelled_input ~type_:Int "Anzahl"
  and description_f = labelled_input "Beschreibung"
  and price_f = labelled_input ~type_:Monetary "Einzelpreis"
  and tax_f = labelled_input ~type_:Int "Steuer"
  and sum_f = labelled_input ~type_:Monetary "Summe"
  and sum_row_f = labelled_input ~type_:Monetary "Preis"
  and deposit_f = labelled_input ~type_:Monetary "Anzahlung"
  and sum'_f = labelled_input ~type_:Monetary "Nach Anzahlung" in
  let%map data = model >>| Model.local
  and s = model >>| Fn.compose Invoice.summary Model.local in
  let input ?(nth = 0) lbl value action =
    lbl ~nth ?on_input:(Some (Fn.compose inject action)) value
  and no_input ?(nth = 0) lbl value = lbl ~nth ?on_input:None value
  and tax_table =
    let open Node in
    table
      [ Attr.classes [ "table"; "table-sm" ] ]
      [ thead
          []
          [ tr
              []
              [ th [ Attr.create "colspan" "2" ] [ text "Enthaltene Mehrwertsteuer" ] ]
          ]
      ; tbody
          []
          (List.map s.included_tax ~f:(fun (rate, value) ->
               tr
                 []
                 [ td [] [ text (string_of_int rate); text "%" ]
                 ; td [] [ text (Monetary.to_string value); text "€" ]
                 ]))
      ]
  and letter_btn data =
    let href = Letter.(invoice (Model.strip_empty_pos data) |> href) in
    Bs.button' ~attr:Attr.[ create "target" "_blank" ] ~href "Drucken"
  and reload_btn = Bs.button ~action:(fun _ -> env.reload ()) "Daten übernehmen" in
  let rows =
    let open Bs.Grid in
    [ frow
        [ col4 [ input recipient_f data.recipient Action.recipient ]
        ; col4 []
        ; col4
            [ input id_f (Option.value ~default:"" data.id) Action.id
            ; input
                date_f
                (Option.value ~default:(Ext_date.today ()) data.date |> Date.to_string)
                Action.date
            ]
        ]
    ; frow [ col [ input title_f data.title Action.title ] ]
    ; frow [ col [ input intro_f data.intro Action.intro ] ]
    ]
    @ List.mapi data.positions ~f:(fun nth p ->
          frow
            [ col1
                [ input
                    quantity_f
                    ~nth
                    (string_of_int p.quantity)
                    (Fn.compose (Action.position nth) Action.quantity)
                ]
            ; col6
                [ input
                    description_f
                    ~nth
                    p.description
                    (Fn.compose (Action.position nth) Action.description)
                ]
            ; col1
                [ input
                    tax_f
                    ~nth
                    (string_of_int p.tax)
                    (Fn.compose (Action.position nth) Action.tax)
                ]
            ; col2
                [ input
                    price_f
                    ~nth
                    (Monetary.to_string_dot p.price)
                    (Fn.compose (Action.position nth) Action.price)
                ]
            ; col2
                [ no_input
                    sum_row_f
                    ~nth
                    Monetary.(to_string_dot (times p.quantity p.price))
                ]
            ])
    @ [ frow
          [ col4 [ tax_table ]
          ; col6 []
          ; col2
              [ no_input sum_f (Monetary.to_string_dot s.sum)
              ; input deposit_f (Monetary.to_string_dot data.deposit) Action.deposit
              ; no_input sum'_f Monetary.(s.sum - data.deposit |> to_string_dot)
              ]
          ]
      ; frow [ col [ input closing_f data.closing Action.closing ] ]
      ; frow
          ~c:[ "mt-2"; "mb-2" ]
          [ col_auto [ reload_btn ]
          ; col [ frow ~c:[ "justify-content-end" ] [ col_auto [ letter_btn data ] ] ]
          ]
      ]
  in
  Node.create "form" [] rows
;;

let create ~env
           ~(inject : Action.t -> Vdom.Event.t)
           (model : Model.t Incr.t) =
  let%map model = model
  and view = view ~env ~inject model in
  let apply_action = apply_action model
  and extra = [] in
  Component.create_with_extra ~extra ~apply_action model view
;;