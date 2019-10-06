(* TODO:
   - use Incr_dom_widgets.Interactive (also on invoice)
   - (re)add no_tax checkbox
   - allow invoice reset
*)
open Base
open Incr_dom
open Ghm
open Interfaces

type env =
  { nav : Nav.booking Incr.t
  ; customer : Customer.t Incr.t
  ; new_booking : Booking.t inject
  ; delete_booking : unit inject
  }

module Model = struct
  open Booking

  type t =
    { local : Booking.t
    ; invoice : Invoice_form.Model.t
    }
  [@@deriving compare, fields]

  let load b =
    { local = b
    ; invoice = Invoice_form.Model.load (Option.value ~default:Invoice.empty b.invoice)
    }
  ;;
end

module Action = struct
  type alloc =
    | Room of string
    | Price_per_bed of string
    | Beds of string
    | Description of string
  [@@deriving sexp_of, variants]

  type guest =
    | Given of string
    | Family of string
    | Born of string
  [@@deriving sexp_of, variants]

  type booking =
    | PeriodA of string
    | PeriodB of string
    | Deposit_asked of string
    | Deposit_got of string
    | Tax_free of bool
    | Note of string
    | Guest of int * guest
    | Alloc of int * alloc
    | DeleteGuest of int
    | DeleteAlloc of int
    | NewGuest
    | NewAlloc
  [@@deriving sexp_of, variants]

  type t =
    | Booking of booking
    | Invoice of Invoice_form.Action.t
  [@@deriving sexp_of, variants]
end

module P = struct
  let string s = String.strip s

  let int s =
    match Int.of_string (string s) with
    | f -> Some f
    | exception _ -> None
  ;;

  let float s =
    match Float.of_string (string s) with
    | f -> Some f
    | exception _ -> None
  ;;

  let date s =
    match Date_yojson.of_string (string s) with
    | f -> Some f
    | exception _ -> None
  ;;

  let monetary s = Option.bind ~f:Monetary.of_float (float s)

  let opt f s =
    match string s with
    | "" -> None
    | s -> Some (f s)
  ;;
end

module S = struct
  let int x = Int.to_string x
  let _float x = Float.to_string x
  let monetary x = Monetary.to_string_dot x
  let date x = Date_yojson.to_string x

  let opt f = function
    | None -> ""
    | Some x -> f x
  ;;
end

let apply_action
    ~invoice
    (model : Model.t)
    (action : Action.t)
    (state : State.t)
    ~schedule_action
    : Model.t
  =
  match action with
  | Booking action ->
    let open Booking in
    let local =
      let x = model.local in
      match action with
      | PeriodA s ->
        (match P.date s with
        | Some a -> { x with period = Period.update ~a x.period }
        | None -> x (* TODO: handle *))
      | PeriodB s ->
        (match P.date s with
        | Some b -> { x with period = Period.update ~b x.period }
        | None -> x (* TODO: handle *))
      | Deposit_asked s ->
        (match P.(opt monetary) s with
        | Some deposit_asked -> { x with deposit_asked }
        | None -> x (* TODO: handle *))
      | Deposit_got s ->
        (match P.(opt monetary) s with
        | Some deposit_got -> { x with deposit_got }
        | None -> x (* TODO: handle *))
      | Tax_free tax_free -> { x with tax_free }
      | Note s -> { x with note = P.string s }
      | Alloc (i, action) ->
        let f x =
          match action with
          | Room s ->
            let room = P.string s in
            { x with room }
          | Price_per_bed s ->
            (match P.monetary s with
            | Some price_per_bed -> { x with price_per_bed }
            | None -> x (* TODO: handle *))
          | Beds s ->
            (match P.int s with
            | Some beds -> { x with beds }
            | None -> x (* TODO: handle *))
          | Description s ->
            let description = P.string s in
            { x with description }
        in
        { x with allocs = List.mapi ~f:(fun j x -> if i = j then f x else x) x.allocs }
      | Guest (i, action) ->
        let f x =
          match action with
          | Given s -> { x with given = P.string s }
          | Family s -> { x with family = P.string s }
          | Born s ->
            (match P.(opt date) s with
            | Some born -> { x with born }
            | None -> x (* TODO: handle *))
        in
        { x with guests = List.mapi ~f:(fun j x -> if i = j then f x else x) x.guests }
      | DeleteGuest i -> { x with guests = List.filteri ~f:(fun j _ -> i <> j) x.guests }
      | DeleteAlloc i -> { x with allocs = List.filteri ~f:(fun j _ -> i <> j) x.allocs }
      | NewGuest -> { x with guests = x.guests @ [ Booking.empty_guest ] }
      | NewAlloc -> { x with allocs = x.allocs @ [ Booking.empty_alloc ] }
    in
    { model with local }
  | Invoice action ->
    let schedule_action = Fn.compose schedule_action Action.invoice in
    { model with invoice = Component.apply_action ~schedule_action invoice action state }
;;

open Vdom
open Vdom_form

(* TODO: use Incr_dom_widgets.Interactive *)
module F = struct
  let from = labelled_input ~type_:Date "Von"
  let till = labelled_input ~type_:Date "Bis"
  let deposit_asked = labelled_input ~type_:Monetary "Anzahlung gefordert"
  let deposit_got = labelled_input "Anzahlung erhalten"

  (* let tax_free =  *)
  let note = labelled_textfield ~rows:8 "Notiz"
  let room = labelled_input "Nr."
  let price_per_bed = labelled_input ~type_:Monetary "Preis"
  let beds = labelled_input ~type_:Int "Betten"
  let decription = labelled_input "Beschreibung"
  let given = labelled_input "Vorname"
  let family = labelled_input "Nachname"
  let born = labelled_input ~type_:Date "Geburtsdatum"
end

open Incr.Let_syntax

let view ~env ~inject (model : Model.t Incr.t) =
  (* TODO: share with invoice_form *)
  let input ?(nth = 0) lbl value action =
    lbl ~nth ?on_input:(Some (Fn.compose inject action)) value
  in
  let act f _ = inject f in
  let delete_button action title =
    Bs.button
      ~i:(R "trash-alt")
      ~style:"outline-danger"
      ~attr:[ Bs.tab_skip ]
      ~action:(act action)
      title
  in
  let%map data = model >>| Model.local
  and customer = env.customer in
  let guests =
    (Node.h4 [] [ Node.text "Gäste" ]
    :: List.concat_mapi data.guests ~f:(fun nth x ->
           let a f = Fn.compose (Action.guest nth) f in
           let input = input ~nth in
           Node.hr []
           :: Bs.Grid.
                [ frow
                    [ col [ input F.given x.given (a Action.given) ]
                    ; col [ input F.family x.family (a Action.family) ]
                    ]
                ; frow
                    [ col [ input F.born (S.(opt date) x.born) (a Action.born) ]
                    ; col
                        ~c:[ "align-self-end"; "text-right" ]
                        [ delete_button (Action.deleteguest nth) "Gast löschen" ]
                    ]
                ]))
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

let create ~env
           ~(inject : Action.t -> Vdom.Event.t)
           (model : Model.t Incr.t) =
  let invoice =
    let inject = Fn.compose inject Action.invoice
    and model = model >>| Model.invoice in
    Invoice_form.create ~env:() ~inject model
  in
  let%map model = model
  and bdata_view =
    let inject = Fn.compose inject Action.booking in
    view ~env ~inject model
  and invoice_view = invoice >>| Component.view
  and nav = env.nav
  and invoice = invoice in
  let view =
    match nav with
    | BData -> bdata_view
    | Invoice -> invoice_view
  in
  let apply_action = apply_action ~invoice model in
  let extra =
    let b = model.local in
    let invoice = Some (Component.extra invoice) (* TODO: filter empty invoice *) in
    { b with invoice }
  in
  Component.create_with_extra ~apply_action ~extra model view
;;
