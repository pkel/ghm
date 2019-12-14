open Core_kernel
open Ghm
open Invoice

module Model = struct

  type t =
    { init : int * Invoice.t
    ; cache : Invoice.t
    }
  [@@deriving compare, fields]

  let append_empty_pos inv = { inv with positions = inv.positions @ [ empty_position ] }

  let strip_empty_pos inv =
    { inv with positions = List.filter ~f:(fun p -> p.description <> "") inv.positions }
  ;;

  let ensure_empty_last m =
    match List.last m.cache.positions with
    | Some e when e <> empty_position -> { m with cache = append_empty_pos m.cache }
    | None -> { m with cache = append_empty_pos m.cache }
    | _ -> m
  ;;
end

(* TODO: Do we need this nonce? *)
let init cache = { Model.init = Nonce.int (), cache; cache}

let eval model =
  let open Model in
  let x = model.cache in
  Ok x
;;

module Action = struct
  type position =
    | Quantity of int
    | Description of string
    | Price of Monetary.t
    | Tax of int
  [@@deriving sexp, variants]

  type t =
    | Recipient of string
    | Title of string
    | Date of Date.t option
    | Id of string option
    | Position of int * position
    | Deposit of Monetary.t
    | Intro of string
    | Closing of string
  [@@deriving sexp, variants]
end

let apply_action model =
  let open Model in
  let open Action in
  let open Invoice in
  let x = model.cache in
  let _init cache = { cache; init = fst model.init, cache }
  and cache cache = { model with cache }
  in
  function
  | Recipient recipient -> cache { x with recipient }
  | Title title -> cache { x with title }
  | Intro intro -> cache { x with intro }
  | Closing closing -> cache { x with closing }
  | Id id -> cache { x with id }
  | Date date -> cache { x with date }
  | Deposit deposit -> cache { x with deposit }
  | Position (i, action) ->
    let f x =
      match action with
      | Description description -> { x with description }
      | Quantity quantity -> { x with quantity }
      | Tax tax -> { x with tax }
      | Price price -> { x with price }
    in
    cache { x with positions = List.mapi ~f:(fun j x -> if i= j then f x else x) x.positions }

let apply_action model action _state ~schedule_action:_ = apply_action model action

open Action
open Incr_dom
open Vdom
open Bs.Form
open Incr_dom_widgets.Interactive
open Incr.Let_syntax

let ignore =
  let inject _ = Event.Ignore
  and on_input _ = () in
  render ~inject ~on_input

let position ~inject ~nth ~(init: Invoice.position) cache =
  let x = init in
  let input on_input =
    let inject a = inject (Position (nth, a)) in
    render ~inject ~on_input
  in
  let%map tax = input tax (int ~init:x.tax ~label:"Steuer" ())
  and description = input description (string ~init:x.description ~label:"Beschreibung" ())
  and quantity = input quantity (int ~init:x.quantity ~label:"Anzahl" ())
  and price = input price (monetary ~init:x.price ~label:"Einzelpreis" ())
  and sum =
    let%bind cache = cache in
    let init =
      Monetary.times cache.quantity cache.price
    in
    ignore (monetary ~init ~label:"Preis" ())
  in
  let open Bs.Grid in
  frow
    [ col1 [ quantity ]
    ; col6 [ description ]
    ; col1 [ tax ]
    ; col2 [ price ]
    ; col2 [ sum ]
    ]

let invoice ~inject ~(init: Invoice.t) cache =
  let x = init in
  (* TODO: WIP here *)
;;

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

let create ~env ~(inject : Action.t -> Vdom.Event.t) (model : Model.t Incr.t) =
  let%map model = model
  and view = view ~env ~inject model in
  let apply_action = apply_action model
  and extra = [] in
  Component.create_with_extra ~extra ~apply_action model view
;;
