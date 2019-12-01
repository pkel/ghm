(* TODO:
   - use Incr_dom_widgets.Interactive (also on invoice)
   - (re)add no_tax checkbox
*)
open Base
open Incr_dom
open Ghm
open Interfaces

type env =
  { nav : Nav.booking Incr.t
  ; rel : Nav.booking -> Nav.main
  ; customer : Customer.t Incr.t
  ; customer_id : Nav.noi Incr.t
  }

module Model = struct
  type t =
    { init : int * Booking.t
    ; cache : Booking.t
    }
  [@@deriving compare, fields]
end

let init cache = { Model.init = Nonce.int (), cache; cache }

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

  type t =
    | PeriodA of Date.t
    | PeriodB of Date.t
    | Deposit_asked of Monetary.t option
    | Deposit_got of Monetary.t option
    | Tax_free of bool
    | Note of string
    | Guest of int * guest
    | Alloc of int * alloc
    | DeleteGuest of int
    | DeleteAlloc of int
    | NewGuest
    | NewAlloc
  [@@deriving sexp_of, variants]
end

let apply_action x =
  let open Action in
  let open Booking in
  function
  | PeriodA a -> { x with period = Period.update ~a x.period }
  | PeriodB b -> { x with period = Period.update ~b x.period }
  | Deposit_asked deposit_asked -> { x with deposit_asked }
  | Deposit_got deposit_got -> { x with deposit_got }
  | Tax_free tax_free -> { x with tax_free }
  | Note note -> { x with note }
  | Alloc (i, action) ->
    let f x =
      match action with
      | Room room -> { x with room }
      | Price_per_bed price_per_bed -> { x with price_per_bed }
      | Beds beds -> { x with beds }
      | Description description -> { x with description }
    in
    { x with allocs = List.mapi ~f:(fun j x -> if i = j then f x else x) x.allocs }
  | Guest (i, action) ->
    let f x =
      match action with
      | Given given -> { x with given }
      | Family family -> { x with family }
      | Born born -> { x with born }
    in
    { x with guests = List.mapi ~f:(fun j x -> if i = j then f x else x) x.guests }
  | DeleteGuest i -> { x with guests = List.filteri ~f:(fun j _ -> i <> j) x.guests }
  | DeleteAlloc i -> { x with allocs = List.filteri ~f:(fun j _ -> i <> j) x.allocs }
  | NewGuest -> { x with guests = x.guests @ [ Booking.empty_guest ] }
  | NewAlloc -> { x with allocs = x.allocs @ [ Booking.empty_alloc ] }
;;

let apply_action model action _state ~schedule_action:_ =
  Model.{ model with cache = apply_action model.cache action }
;;

open Action
open Bs.Form
open Incr_dom
open Incr_dom_widgets.Interactive
open Incr.Let_syntax

let delete_button action title =
  Bs.button ~i:(R "trash-alt") ~style:"outline-danger" ~attr:[ Bs.tab_skip ] ~action title
;;

let alloc ~inject ~nth ~(init : Booking.alloc) =
  let x = init in
  let input on_input =
    let inject a = inject (Alloc (nth, a)) in
    render ~inject ~on_input
  and delete =
    delete_button (fun _ -> inject (Action.deletealloc nth)) "Zimmer löschen"
  in
  let%map room = input room (string ~init:x.room ~label:"Nr." ())
  and description =
    input description (string ~init:x.description ~label:"Beschreibung" ())
  and beds = input beds (int ~init:x.beds ~label:"Betten" ())
  and price_per_bed =
    input price_per_bed (monetary ~init:x.price_per_bed ~label:"Preis" ())
  in
  Bs.Grid.
    [ frow [ col3 [ room ]; col9 [ description ] ]
    ; frow
        [ col4 [ beds ]
        ; col4 [ price_per_bed ]
        ; col4 ~c:[ "align-self-end"; "text-right" ] [ delete ]
        ]
    ]
;;

let guest ~inject ~nth ~(init : Booking.guest) =
  let x = init in
  let input on_input =
    let inject a = inject (Guest (nth, a)) in
    render ~inject ~on_input
  and delete = delete_button (fun _ -> inject (Action.deleteguest nth)) "Gast löschen" in
  let%map given = input given (string ~init:x.given ~label:"Vorname" ())
  and family = input family (string ~init:x.family ~label:"Nachname" ())
  and born = input born (date_opt ~init:x.born ~label:"Geburtsdatum" ()) in
  Bs.Grid.
    [ frow [ col [ given ]; col [ family ] ]
    ; frow [ col [ born ]; col ~c:[ "align-self-end"; "text-right" ] [ delete ] ]
    ]
;;

let view ~env ~inject (model : Model.t Incr.t) =
  (* TODO: share with invoice_form *)
  let input ?(nth = 0) lbl value action =
    lbl ~nth ?on_input:(Some (Fn.compose inject action)) value
  in
  let act f _ = inject f in
  let%map data = model >>| Model.local
  and customer = env.customer in
  let guests =
    (Node.h4 [] [ Node.text "Gäste" ]
    :: List.concat_mapi data.guests ~f:(fun nth x ->
           let a f = Fn.compose (Action.guest nth) f in
           let input = input ~nth in
           Node.hr [] :: alloc ~inject ~nth ~init:x))
    @ [ Node.hr []
      ; Node.div
          []
          [ Bs.button ~i:(S "plus") ~action:(act Action.newguest) "Weiterer Gast" ]
      ]
  and allocs =
    (Node.h4 [] [ Node.text "Positionen" ]
    :: List.concat_mapi data.allocs ~f:(fun nth x ->
           let a f = Fn.compose (Action.alloc nth) f in
           let input = input ~nth in
           Node.hr []
           :: Bs.Grid.
                [ frow
                    [ col3 [ input F.room x.room (a Action.room) ]
                    ; col9 [ input F.decription x.description (a Action.description) ]
                    ]
                ; frow
                    [ col4 [ input F.beds (S.int x.beds) (a Action.beds) ]
                    ; col4
                        [ input
                            F.price_per_bed
                            (S.monetary x.price_per_bed)
                            (a Action.price_per_bed)
                        ]
                    ; col4
                        ~c:[ "align-self-end"; "text-right" ]
                        [ delete_button (Action.deletealloc nth) "Zimmer löschen" ]
                    ]
                ]))
    @ [ Node.hr []
      ; Node.div
          []
          [ Bs.button ~i:(S "plus") ~action:(act Action.newalloc) "Weitere Position" ]
      ]
  and main =
    Bs.Grid.
      [ Node.h4 [] [ Node.text "Aufenthalt" ]
      ; Node.hr []
      ; frow
          [ col [ input F.from (S.date (Period.a data.period)) Action.perioda ]
          ; col [ input F.till (S.date (Period.b data.period)) Action.periodb ]
          ]
      ; frow
          [ col
              [ input
                  F.deposit_asked
                  S.(opt monetary data.deposit_asked)
                  Action.deposit_asked
              ]
          ; col
              [ input F.deposit_got S.(opt monetary data.deposit_got) Action.deposit_got ]
          ]
        (* ; frow [ col ~c:[ "mb-2" ] [ input_bool state "befreit von Kurtaxe" tax_free ] ] *)
      ; frow [ col [ input F.note data.note Action.note ] ]
      ]
  in
  let excel = Excel_br_2014_v2.of_customer_and_booking customer data
  and confirmation =
    let date = Browser.Date.(now () |> to_locale_date_string) in
    Letter.(confirm ~booking:data ~date customer |> href)
  and danger_btn action title =
    (* TODO: reuse *)
    Bs.button ~attr:[ Bs.tab_skip ] ~style:"outline-danger" ~action title
  in
  Node.div
    [] (* TODO: double check whether div makes sense *)
    Bs.Grid.
      [ row [ col main; col allocs; col guests ]
      ; frow
          [ col_auto
              ~c:[ "mb-2"; "mt-2" ]
              [ Bs.button' ~href:confirmation ~blank:true "Bestätigung" ]
          ; col_auto ~c:[ "mb-2"; "mt-2" ] [ Bs.button_clipboard ~value:excel "Excel" ]
          ; col
              [ frow
                  ~c:[ "justify-content-end" ]
                  [ col_auto
                      ~c:[ "mb-2"; "mt-2" ]
                      [ danger_btn (fun _ -> env.delete_booking ()) "Buchung löschen" ]
                  ; col_auto
                      ~c:[ "mb-2"; "mt-2" ]
                      [ Bs.button ~action:(fun _ -> env.new_booking data) "Neue Buchung" ]
                  ]
              ]
          ]
      ]
;;

(* TODO: use Incr_dom_widgets.Interactive *)
module F = struct
  let from = labelled_input ~type_:Date "Von"
  let till = labelled_input ~type_:Date "Bis"
  let deposit_asked = labelled_input ~type_:Monetary "Anzahlung gefordert"
  let deposit_got = labelled_input "Anzahlung erhalten"

  (* let tax_free =  *)
  let note = labelled_textfield ~rows:8 "Notiz"
  let room = labelled_input "Nr."
  let given = labelled_input "Vorname"
  let family = labelled_input "Nachname"
  let born = labelled_input ~type_:Date "Geburtsdatum"
end

let create ~env ~(inject : Action.t -> Vdom.Event.t) (model : Model.t Incr.t) =
  let invoice =
    let inject = Fn.compose inject Action.invoice
    and model = model >>| Model.invoice
    and env = { Invoice_form.reload = (fun () -> inject Action.Reload_invoice) } in
    Invoice_form.create ~env ~inject model
  in
  let%map model = model
  and bdata_view =
    let inject = Fn.compose inject Action.booking in
    view ~env ~inject model
  and invoice_view = invoice >>| Component.view
  and nav = env.nav
  and invoice = invoice
  and customer = env.customer in
  let view =
    match nav with
    | BData -> bdata_view
    | Invoice -> invoice_view
  in
  let apply_action = apply_action ~customer ~invoice model
  and extra =
    let open Menu in
    let rel x = Href (Nav.href (env.rel x)) in
    [ entry "Eingabe" (rel Nav.BData) (phys_equal nav Nav.BData)
    ; entry "Rechnung" (rel Nav.Invoice) (phys_equal nav Nav.Invoice)
    ]
  in
  Component.create_with_extra ~extra ~apply_action model view
;;
