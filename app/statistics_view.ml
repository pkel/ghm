open Core_kernel
open Incr_dom

module Model = struct
  type t =
    { bookings : [ `Ok of Pg.Bookings.return list | `Loading | `Failed ]
    ; date : Date.t
    ; date_init : Date.t
    }
  [@@deriving compare, fields]

  let create () =
    let today = Ext_date.today () in
    { bookings = `Loading; date = today; date_init = today }
  ;;

  let period t =
    let start =
      let open Date in
      let d = t.date in
      create_exn ~y:(year d) ~m:(month d) ~d:1
    and end_ =
      let open Date in
      let d = add_months t.date 1 in
      add_days (create_exn ~y:(year d) ~m:(month d) ~d:1) (-1)
    in
    Ghm.Period.of_dates start end_
  ;;
end

module Action = struct
  type t =
    | Next_month
    | Prev_month
    | Date of Date.t
    | Get
    | Got of Pg.Bookings.return list sexp_opaque Or_error.t
  [@@deriving sexp_of, variants]

  let refresh = Get
end

let apply_action (model : Model.t) (action : Action.t) (state : State.t) ~schedule_action
  =
  let init m = Model.{ m with date_init = m.date } in
  match action with
  | Got (Error detail) ->
    state.handle_error { gist = "Laden fehlgeschlagen"; detail };
    { model with bookings = `Failed }
  | Got (Ok l) -> { model with bookings = `Ok l }
  | Get ->
    let c = state.connection
    and handler = Fn.compose schedule_action Action.got
    and request =
      let open Pg in
      let filter =
        let p = Model.period model in
        let open Date in
        let open Bookings in
        arrival <= Ghm.Period.till p && departure > Ghm.Period.from p
      in
      read ~filter Bookings.t
    in
    Xhr.send' ~c ~handler request;
    { model with bookings = `Loading }
  | Date date ->
    schedule_action Action.Get;
    { model with date }
  | Next_month ->
    schedule_action Action.Get;
    init { model with date = Date.add_months model.date 1 }
  | Prev_month ->
    schedule_action Action.Get;
    init { model with date = Date.add_months model.date (-1) }
;;

open Vdom
open Incr.Let_syntax
open Ghm

let row ~p (booking : Pg.Bookings.return) =
  let n_guests = List.length booking.data.guests
  and from = Period.from booking.data.period
  and till = Period.till booking.data.period
  and link = Nav.href (Customer (Id booking.customer.id, Booking (Id booking.id))) in
  let arrival = if Date.compare from (Period.from p) < 0 then 0 else n_guests
  and this, overflow =
    List.fold
      ~init:(0, 0)
      ~f:(fun (t, o) d ->
        if Date.compare (Period.till p) d >= 0 then t + n_guests, o else t, o + n_guests)
      (Date.dates_between
         ~min:(Date.max from (Period.from p))
         ~max:(Date.add_days till (-1)))
  in
  let date, no, who, where =
    let from_invoice =
      match booking.data.invoice with
      | Some i ->
        let l = String.split_lines i.recipient in
        (match List.hd l, List.last l with
        | Some name, Some l ->
          let cc =
            match String.split ~on:'-' l with
            | [ _ ] | [] -> booking.customer.data.address.country_code
            | hd :: _tl -> hd
          and no = Option.value ~default:"" i.id
          and date = Option.map ~f:Localize.date i.date |> Option.value ~default:"" in
          Some (date, no, name, cc)
        | _, _ -> None)
      | None -> None
    in
    match from_invoice with
    | Some x -> x
    | None ->
      "", "", booking.customer.keyword, booking.customer.data.address.country_code
  in
  let open Node in
  let open Attr in
  ( where
  , arrival
  , this
  , tr
      []
      [ td [] [ a [ href link ] [ text "Öffnen" ] ]
      ; td [] [ text no ]
      ; td [] [ text date ]
      ; td [] [ text who ]
      ; td [] [ text (Localize.date from) ]
      ; td [] [ text (Localize.date till) ]
      ; td [] [ text where ]
      ; td [] [ text (string_of_int arrival) ]
      ; td [] [ text (string_of_int this) ]
      ; td [] [ text (string_of_int overflow) ]
      ] )
;;

let summary stats =
  let a_, n, r =
    String.Map.fold
      ~init:(0, 0, [])
      ~f:(fun ~key ~data (aa, nn, rr) ->
        let a', n = data in
        let r =
          let open Node in
          tr
            []
            [ td [] [ text key ]
            ; td [] [ text (string_of_int a') ]
            ; td [] [ text (string_of_int n) ]
            ]
        in
        aa + a', nn + n, r :: rr)
      stats
  in
  let open Node in
  let open Attr in
  table
    [ classes [ "table"; "table-hover"; "table-sm" ] ]
    [ thead
        []
        [ tr
            []
            [ th [ create "scope" "col" ] [ text "Land" ]
            ; th [ create "scope" "col" ] [ text "angereist" ]
            ; th [ create "scope" "col" ] [ text "Nächte" ]
            ]
        ]
    ; tbody [] r
    ; tbody
        []
        [ tr
            []
            [ th [ create "scope" "row" ] [ text "Summe" ]
            ; td [] [ text (string_of_int a_) ]
            ; td [] [ text (string_of_int n) ]
            ]
        ]
    ]
;;

let statistics ~p ~bookings =
  let bookings =
    List.filter_map
      ~f:(fun (b : Pg.Bookings.return) ->
        match b.data.invoice with
        | Some i -> Some (i.id, b)
        | None -> None)
      bookings
    |> List.sort ~compare:(fun (a, _) (b, _) -> compare a b)
    |> List.map ~f:(fun (_, x) -> x)
  in
  let stats, bookings_rows =
    List.fold bookings ~init:(String.Map.empty, []) ~f:(fun (m, l) b ->
        let where, arrival, nights, row = row ~p b in
        let old_arrival, old_night =
          String.Map.find m where |> Option.value ~default:(0, 0)
        in
        ( String.Map.set ~key:where ~data:(old_arrival + arrival, old_night + nights) m
        , row :: l ))
  in
  let booking_table =
    let open Node in
    let open Attr in
    table
      [ classes [ "table"; "table-hover"; "table-sm" ] ]
      [ thead
          []
          [ tr
              []
              [ th [ create "scope" "col" ] [ text "" ]
              ; th [ create "scope" "col" ] [ text "RNr." ]
              ; th [ create "scope" "col" ] [ text "Datum" ]
              ; th [ create "scope" "col" ] [ text "Name" ]
              ; th [ create "scope" "col" ] [ text "Von" ]
              ; th [ create "scope" "col" ] [ text "Bis" ]
              ; th [ create "scope" "col" ] [ text "Land" ]
              ; th [ create "scope" "col" ] [ text "Angereist" ]
              ; th [ create "scope" "col" ] [ text "Nächte" ]
              ; th [ create "scope" "col" ] [ text "Übertrag" ]
              ]
          ]
      ; tbody [] bookings_rows
      ]
  in
  let open Bs.Grid in
  [ row
      [ col
          [ Node.h3
              []
              [ Node.text
                  (sprintf
                     "Statistik %s %d"
                     (Date.month (Period.from p) |> Localize.month)
                     (Date.year (Period.from p)))
              ]
          ; summary stats
          ; Node.h4 [] [ Node.text "Rechnungen" ]
          ; booking_table
          ]
      ]
  ]
;;

let datepicker ~inject ~init =
  let interactive =
    let prepend =
      [ Bs.button
          (Icon (S "step-backward", "Vorheriger Monat"))
          (Action (fun _ -> inject Action.prev_month))
      ]
    and append =
      [ Bs.button
          (Icon (S "step-forward", "Nächster Monat"))
          (Action (fun _ -> inject Action.next_month))
      ]
    in
    Bs.Form.date ~init ~prepend ~append ()
  in
  let%map field =
    Incr_dom_widgets.Interactive.render ~on_input:Action.date ~inject interactive
  in
  Node.create "form" [ Attr.classes [ "mt-2"; "mb-4" ] ] [ field ]
;;

let view ~inject model =
  let%map statistics =
    let%bind p = model >>| Model.period in
    model
    >>| Model.bookings
    >>| function
    | `Ok bookings -> statistics ~p ~bookings
    | _ -> [ Bs.Grid.loading_row ]
  and datepicker =
    let%bind init = model >>| Model.date_init in
    datepicker ~inject ~init
  in
  Node.div [] (datepicker :: statistics)
;;

let create ~inject model =
  let%map model = model
  and view = view ~inject model in
  let apply_action = apply_action model in
  Component.create ~apply_action model view
;;
