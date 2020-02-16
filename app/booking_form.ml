open Base
open Ghm

module Model = struct
  type t =
    { initital : int * Booking.t
    ; cache : Booking.t
    }
  [@@deriving compare, fields]

  (* TODO: do we need this nonce? *)
  let init cache = { initital = Nonce.int (), cache; cache }
end

let init = Model.init

let eval model =
  let open Model in
  let x = model.cache in
  Ok x
;;

module Action = struct
  type alloc =
    | Room of string
    | Price_per_bed of Monetary.t
    | Beds of int
    | Description of string
  [@@deriving sexp_of, variants]

  type guest =
    | Given of string
    | Family of string
    | Born of Date.t option
  [@@deriving sexp_of, variants]

  type 'a list_element =
    | Field of 'a
    | Tool of List_tools.action
  [@@deriving sexp_of, variants]

  type t =
    | PeriodA of Date.t
    | PeriodB of Date.t
    | Deposit_asked of Monetary.t option
    | Deposit_got of Monetary.t option
    | Tax_free of bool
    | Note of string
    | Guest of int * guest list_element
    | NewGuest
    | Alloc of int * alloc list_element
    | NewAlloc
  [@@deriving sexp_of, variants]
end

let apply_action model =
  let open Model in
  let open Action in
  let open Booking in
  let x = model.cache in
  let cache cache = { model with cache } in
  function
  | PeriodA a -> cache { x with period = Period.update ~a x.period }
  | PeriodB b -> cache { x with period = Period.update ~b x.period }
  | Deposit_asked deposit_asked -> cache { x with deposit_asked }
  | Deposit_got deposit_got -> cache { x with deposit_got }
  | Tax_free tax_free -> cache { x with tax_free }
  | Note note -> cache { x with note }
  | Alloc (i, Field action) ->
    let f x =
      match action with
      | Room room -> { x with room }
      | Price_per_bed price_per_bed -> { x with price_per_bed }
      | Beds beds -> { x with beds }
      | Description description -> { x with description }
    in
    cache { x with allocs = List.mapi ~f:(fun j x -> if i = j then f x else x) x.allocs }
  | Guest (i, Field action) ->
    let f x =
      match action with
      | Given given -> { x with given }
      | Family family -> { x with family }
      | Born born -> { x with born }
    in
    cache { x with guests = List.mapi ~f:(fun j x -> if i = j then f x else x) x.guests }
  | Alloc (i, Tool a) -> init { x with allocs = List_tools.apply_action x.allocs i a }
  | Guest (i, Tool a) -> init { x with guests = List_tools.apply_action x.guests i a }
  | NewAlloc -> init { x with allocs = x.allocs @ [ Booking.empty_alloc ] }
  | NewGuest -> init { x with guests = x.guests @ [ Booking.empty_guest ] }
;;

let apply_action model action _state ~schedule_action:_ = apply_action model action

open Action
open Incr_dom
open Vdom
open Bs.Form
open Incr_dom_widgets.Interactive
open Incr.Let_syntax

let alloc ~inject ~nth ~(init : Booking.alloc) =
  let x = init in
  let input on_input =
    let inject a = inject (Action.alloc nth (Field a)) in
    render ~inject ~on_input
  and el_tools =
    let inject a = inject (Action.alloc nth (Tool a)) in
    List_tools.view ~inject "Zimmer"
  in
  let%map room = input room (string ~init:x.room ~label:"Nr." ())
  and description =
    let datalist = Booking.room_descriptions in
    input description (string ~init:x.description ~datalist ~label:"Beschreibung" ())
  and beds = input beds (int ~init:x.beds ~label:"Betten" ())
  and price_per_bed =
    input price_per_bed (monetary ~init:x.price_per_bed ~label:"Preis" ())
  in
  Bs.Grid.
    [ frow [ col3 [ room ]; col9 [ description ] ]
    ; frow
        [ col4 [ beds ]
        ; col4 [ price_per_bed ]
        ; col4 ~c:[ "align-self-end"; "text-right" ] el_tools
        ]
    ]
;;

let guest ~inject ~nth ~(init : Booking.guest) =
  let x = init in
  let input on_input =
    let inject a = inject (Action.guest nth (Field a)) in
    render ~inject ~on_input
  and el_tools =
    let inject a = inject (Action.guest nth (Tool a)) in
    List_tools.view ~inject "Gast"
  in
  let%map given = input given (string ~init:x.given ~label:"Vorname" ())
  and family = input family (string ~init:x.family ~label:"Nachname" ())
  and born = input born (date_opt ~init:x.born ~label:"Geburtsdatum" ()) in
  Bs.Grid.
    [ frow [ col [ given ]; col [ family ] ]
    ; frow [ col [ born ]; col ~c:[ "align-self-end"; "text-right" ] el_tools ]
    ]
;;

let main ~inject ~(init : Booking.t) =
  let x = init in
  let input on_input = render ~inject ~on_input in
  let%map perioda = input perioda (date ~init:(Period.a x.period) ~label:"Von" ())
  and periodb = input periodb (date ~init:(Period.b x.period) ~label:"Bis" ())
  and deposit_got =
    input deposit_got (monetary_opt ~init:x.deposit_got ~label:"Anzahlung erhalten" ())
  and deposit_asked =
    input
      deposit_asked
      (monetary_opt ~init:x.deposit_asked ~label:"Anzahlung gefordert" ())
  and tax_free =
    input tax_free (checkbox ~init:x.tax_free ~label:"befreit von Kurtaxe" ())
  and note = input note (textarea ~init:x.note ~label:"Notiz" ~nrows:8 ()) in
  Bs.Grid.
    [ Node.h4 [] [ Node.text "Aufenthalt" ]
    ; Node.hr []
    ; frow [ col [ perioda ]; col [ periodb ] ]
    ; frow [ col [ deposit_asked ]; col [ deposit_got ] ]
    ; frow [ col ~c:[ "mb-2" ] [ tax_free ] ]
    ; frow [ col [ note ] ]
    ]
;;

let booking ~inject ~(init : Booking.t) =
  let x = init in
  let act f _ = inject f in
  let%map guests =
    let%map raw =
      List.mapi x.guests ~f:(fun nth init -> guest ~inject ~nth ~init) |> Incr.all
    in
    (Node.h4 [] [ Node.text "Gäste" ]
    :: List.concat_map raw ~f:(fun nodes -> Node.hr [] :: nodes))
    @ [ Node.hr []
      ; Bs.Grid.frow
          ~c:[ "justify-content-end" ]
          [ Bs.Grid.col_auto
              [ Bs.button
                  ~size:`Small
                  (Icon (S "plus", "Weiteren Gast hinzufügen"))
                  (Action (act Action.newguest))
              ]
          ]
      ]
  and allocs =
    let%map raw =
      List.mapi x.allocs ~f:(fun nth init -> alloc ~inject ~nth ~init) |> Incr.all
    in
    (Node.h4 [] [ Node.text "Positionen" ]
    :: List.concat_map raw ~f:(fun nodes -> Node.hr [] :: nodes))
    @ [ Node.hr []
      ; Bs.Grid.frow
          ~c:[ "justify-content-end" ]
          [ Bs.Grid.col_auto
              [ Bs.button
                  ~size:`Small
                  (Icon (S "plus", "Weitere Position hinzufügen"))
                  (Action (act Action.newalloc))
              ]
          ]
      ]
  and main = main ~inject ~init in
  Node.create "form" [] Bs.Grid.[ frow [ col main; col allocs; col guests ] ]
;;

let create ~env:() ~(inject : Action.t -> Vdom.Event.t) (model : Model.t Incr.t) =
  let%map form =
    let%bind init = model >>| Model.initital in
    let init = snd init in
    booking ~inject ~init
  and model = model in
  let apply_action = apply_action model in
  Component.create ~apply_action model form
;;
