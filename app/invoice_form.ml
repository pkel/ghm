open Core_kernel
open Ghm
open Invoice

module Position = struct
  module Model = struct
    type t =
      { init : position
      ; cache : position
      }
    [@@deriving compare, fields]
  end

  open Model

  let init cache = { init = cache; cache }
  let eval model = Ok model.cache

  module Action = struct
    type t =
      | Quantity of int
      | Description of string
      | Price of Monetary.t
      | Tax of int
    [@@deriving sexp, variants]
  end

  open Action

  let apply_action model =
    let open Invoice in
    let x = model.cache in
    let cache cache = { model with cache } in
    function
    | Description description -> cache { x with description }
    | Quantity quantity -> cache { x with quantity }
    | Tax tax -> cache { x with tax }
    | Price price -> cache { x with price }
  ;;

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

  let view ~inject model =
    let%bind x = model >>| Model.init in
    let disabled = true
    and input on_input = render ~inject ~on_input in
    let%map tax = input tax (int ~init:x.tax ~label:"Steuer" ())
    and description =
      input description (string ~init:x.description ~label:"Beschreibung" ())
    and quantity = input quantity (int ~init:x.quantity ~label:"Anzahl" ())
    and price = input price (monetary ~init:x.price ~label:"Einzelpreis" ())
    and sum =
      let%bind cache = model >>| Model.cache in
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
end

module Model = struct
  type t =
    { init : int * Invoice.t
    ; cache : Invoice.t
    ; positions : Position.Model.t Int.Map.t
    }
  [@@deriving compare, fields]

  let map_positions ~f t =
    let positions = f t.positions in
    { t with positions }
  ;;

  let map_position ~f ~nth t =
    match Map.find t.positions nth with
    | None -> t
    | Some old_val ->
      { t with positions = Map.set t.positions ~key:nth ~data:(f old_val) }
  ;;

  let append_pos data =
    let data = Position.init data in
    let f t = Map.set ~key:(Map.length t) ~data t in
    map_positions ~f
  ;;

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

let init cache =
  (* TODO: Do we need this nonce? *)
  let init = { Model.init = Nonce.int (), cache; cache; positions = Int.Map.empty } in
  List.fold_left ~init ~f:(fun acc el -> Model.append_pos el acc) cache.positions
;;

let eval model =
  let open Model in
  let open Result.Let_syntax in
  let%map positions =
    Int.Map.map ~f:Position.eval model.positions |> Int.Map.data |> Result.all
  and invoice = Ok model.cache in
  { invoice with positions }
;;

module Action = struct
  type t =
    | Recipient of string
    | Title of string
    | Date of Date.t option
    | Id of string option
    | Position of int * Position.Action.t
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
  let cache cache = { model with cache } in
  function
  | Recipient recipient -> cache { x with recipient }
  | Title title -> cache { x with title }
  | Intro intro -> cache { x with intro }
  | Closing closing -> cache { x with closing }
  | Id id -> cache { x with id }
  | Date date -> cache { x with date }
  | Deposit deposit -> cache { x with deposit }
  | Position (nth, action) ->
    map_position ~nth ~f:(fun model -> Position.apply_action model action) model
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

module Incr_map = Incr_map.Make (Incr)

let invoice ~inject ~(init : Invoice.t) cache positions =
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
    let f ~key ~data =
      let inject a = inject (Position (key, a)) in
      Position.view ~inject data
    in
    Incr_map.mapi' ~f positions >>| Int.Map.data
  in
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
  @ positions
  @ [ frow [ col4 [ tax_table ]; col6 []; col2 [ sum; deposit; sum_after_deposit ] ]
    ; frow [ col [ closing ] ]
    ]
;;

let create ~env:() ~(inject : Action.t -> Vdom.Event.t) (model : Model.t Incr.t) =
  let%map model = model
  and view =
    let%bind init = model >>| Model.init >>| snd in
    let%map rows =
      invoice ~inject ~init (model >>| Model.cache) (model >>| Model.positions)
    in
    Node.create "form" [] rows
  in
  let apply_action = apply_action model in
  Component.create ~apply_action model view
;;
