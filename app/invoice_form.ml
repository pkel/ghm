open Core_kernel
open Ghm
open Invoice

module Model = struct
  type t =
    { init : int * Invoice.t
    ; cache : Invoice.t
    }
  [@@deriving compare, fields]

  (* TODO: use or delete
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
  *)
end

(* TODO: Do we need this nonce? *)
let init cache = { Model.init = Nonce.int (), cache; cache }

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
  and cache cache = { model with cache } in
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

let ignore =
  let inject _ = Event.Ignore
  and on_input _ = () in
  render ~inject ~on_input
;;

let position ~inject ~nth ~(init : Invoice.position) cache =
  let x = init
  and disabled = true
  and input on_input =
    let inject a = inject (Position (nth, a)) in
    render ~inject ~on_input
  in
  let%map tax = input tax (int ~init:x.tax ~label:"Steuer" ())
  and description =
    input description (string ~init:x.description ~label:"Beschreibung" ())
  and quantity = input quantity (int ~init:x.quantity ~label:"Anzahl" ())
  and price = input price (monetary ~init:x.price ~label:"Einzelpreis" ())
  and sum =
    let%bind cache = cache in
    let init = Monetary.times cache.quantity cache.price in
    ignore (monetary ~init ~disabled ~label:"Preis" ())
  in
  let open Bs.Grid in
  frow
    [ col1 [ quantity ]
    ; col6 [ description ]
    ; col1 [ tax ]
    ; col2 [ price ]
    ; col2 [ sum ]
    ]
;;

let tax_table invoice_summary =
  let open Node in
  table
    [ Attr.classes [ "table"; "table-sm" ] ]
    [ thead
        []
        [ tr [] [ th [ Attr.create "colspan" "2" ] [ text "Enthaltene Mehrwertsteuer" ] ]
        ]
    ; tbody
        []
        (List.map invoice_summary.included_tax ~f:(fun (rate, value) ->
             tr
               []
               [ td [] [ text (string_of_int rate); text "%" ]
               ; td [] [ text (Monetary.to_string value); text "€" ]
               ]))
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
  let%bind cache = cache in
  let s = Invoice.summary cache in
  let tax_table = tax_table s in
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
  @ (let _ = position in
     [])
  (* TODO: positions *)
  @ [ frow [ col4 [ tax_table ]; col6 []; col2 [ sum; deposit; sum_after_deposit ] ]
    ; frow [ col [ closing ] ]
    ]
;;

let create ~env:() ~(inject : Action.t -> Vdom.Event.t) (model : Model.t Incr.t) =
  let%map model = model
  and view =
    let%bind init = model >>| Model.init >>| snd in
    let%map rows = invoice ~inject ~init (model >>| Model.cache) in
    Node.create "form" [] rows
  in
  let apply_action = apply_action model in
  Component.create ~apply_action model view
;;
