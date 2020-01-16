open Core_kernel
open Ghm
open Invoice

module Model = struct
  type t =
    { inititial : int * Invoice.t
    ; cache : Invoice.t
    }
  [@@deriving compare, fields]

  let init cache =
    (* TODO: Do we need this nonce? *)
    { inititial = Nonce.int (), cache; cache }
  ;;
end

let init = Model.init

let eval model =
  let open Model in
  Ok model.cache
;;

module Action = struct
  type position =
    | Quantity of int
    | Description of string
    | Price of Monetary.t
    | Tax of int
  [@@deriving sexp_of, variants]

  type 'a list_element =
    | Field of 'a
    | Tool of List_tools.action
  [@@deriving sexp_of, variants]

  type t =
    | Recipient of string
    | Title of string
    | Date of Date.t option
    | Id of string option
    | Position of int * position list_element
    | Add_position
    | Deposit of Monetary.t
    | Intro of string
    | Closing of string
  [@@deriving sexp_of, variants]
end

let apply_action model =
  let open Model in
  let open Action in
  let open Invoice in
  let x = model.cache in
  let cache cache = { model with cache } in
  function
  | Recipient recipient -> cache { x with recipient }
  | Title title -> cache { x with title }
  | Intro intro -> cache { x with intro }
  | Closing closing -> cache { x with closing }
  | Id id -> cache { x with id }
  | Date date -> cache { x with date }
  | Deposit deposit -> cache { x with deposit }
  | Add_position -> init { x with positions = x.positions @ [ Invoice.empty_position ] }
  | Position (i, Tool a) ->
    init { x with positions = List_tools.apply_action x.positions i a }
  | Position (i, Field action) ->
    let f x =
      match action with
      | Description description -> { x with description }
      | Quantity quantity -> { x with quantity }
      | Tax tax -> { x with tax }
      | Price price -> { x with price }
    in
    cache
      { x with
        positions = List.mapi ~f:(fun j x -> if i = j then f x else x) x.positions
      }
;;

let apply_action model action _state ~schedule_action:_ = apply_action model action

open Action
open Incr_dom
open Vdom
open Bs.Form
open Incr_dom_widgets.Interactive
open Incr.Let_syntax
module Incr_map = Incr_map.Make (Incr)

let ignore =
  let inject _ = Event.Ignore
  and on_input _ = () in
  render ~inject ~on_input
;;

let position_labels =
  let label l = Node.label [] [ Node.text l ] in
  let open Bs.Grid in
  frow
    [ col_auto
        [ Node.span Attr.[ style Css_gen.(display `Inline_block @> width (`Rem 7.7)) ] []
        ]
    ; col [ label "Anzahl" ]
    ; col6 [ label "Beschreibung" ]
    ; col [ label "Steuer" ]
    ; col [ label "Einzelpreis" ]
    ]
;;

let position ~labels ~inject ~nth ~(init : Invoice.position) =
  let int ~init ~label () =
    let label = if labels then Some label else None in
    int ~init ?label ()
  and string ~init ~label () =
    let label = if labels then Some label else None in
    string ~init ?label ()
  and monetary ~init ~label ?disabled () =
    let label = if labels then Some label else None in
    monetary ~init ?disabled ?label ()
  in
  let x = init in
  let input on_input =
    let inject a = inject (Action.position nth (Field a)) in
    render ~inject ~on_input
  and el_tools =
    let inject a = inject (Action.position nth (Tool a)) in
    List_tools.view ~inject "Position"
  in
  let%map tax = input tax (int ~init:x.tax ~label:"Steuer" ())
  and description =
    input description (string ~init:x.description ~label:"Beschreibung" ())
  and quantity = input quantity (int ~init:x.quantity ~label:"Anzahl" ())
  and price = input price (monetary ~init:x.price ~label:"Einzelpreis" ()) in
  let open Bs.Grid in
  frow
    [ col_auto ~c:[ "pt-1" ] el_tools
    ; col [ quantity ]
    ; col6 [ description ]
    ; col [ tax ]
    ; col [ price ]
    ]
;;

let invoice ~inject ~(init : Invoice.t) cache =
  let x = init
  and disabled = true
  and input on_input = render ~inject ~on_input in
  let%bind title = input title (string ~init:x.title ~label:"Titel" ())
  and recipient =
    input recipient (textarea ~nrows:4 ~init:x.recipient ~label:"Empfänger" ())
  and intro = input intro (string ~init:x.intro ~label:"Freitext" ())
  and closing = input closing (string ~init:x.closing ~label:"Freitext" ())
  and date = input Action.date (date_opt ~init:x.date ~label:"Datum" ())
  and id = input id (string_opt ~init:x.id ~label:"Rechnungsnummer" ())
  and deposit = input deposit (monetary ~init:x.deposit ~label:"Anzahlung" ()) in
  let%bind cache = cache
  and positions =
    List.mapi x.positions ~f:(fun nth init -> position ~labels:false ~inject ~nth ~init)
    |> Incr.all
  in
  let add_position =
    Bs.Grid.(
      frow
        [ col
            ~c:[ "mb-2" ]
            [ Bs.button
                ~size:`Small
                (Icon (S "plus", "Position hinzufügen"))
                (Action (fun () -> inject Add_position))
            ]
        ])
  in
  let s = Invoice.summary cache in
  let%map sum = ignore (monetary ~init:s.sum ~label:"Summe" ~disabled ())
  and sum_after_deposit =
    let init =
      let open Monetary in
      s.sum - cache.deposit
    in
    ignore (monetary ~init ~label:"Nach Anzahlung" ~disabled ())
  in
  let open Bs.Grid in
  [ frow [ col4 [ recipient ]; col4 []; col4 [ id; date ] ]
  ; frow [ col [ title ] ]
  ; frow [ col [ intro ] ]
  ]
  @ [ position_labels ]
  @ positions
  @ [ frow [ col4 [ add_position ]; col6 []; col2 [ sum; deposit; sum_after_deposit ] ]
    ; frow [ col [ closing ] ]
    ]
;;

let create ~env:() ~(inject : Action.t -> Vdom.Event.t) (model : Model.t Incr.t) =
  let%map model = model
  and view =
    let%bind init = model >>| Model.inititial >>| snd in
    let%map rows = invoice ~inject ~init (model >>| Model.cache) in
    Node.create "form" [] rows
  in
  let apply_action = apply_action model in
  Component.create ~apply_action model view
;;
