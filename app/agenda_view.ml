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
end

module Action = struct
  type t =
    | Next_day
    | Prev_day
    | Date of Date.t
    | Get
    | Got of Pg.Bookings.return list sexp_opaque Or_error.t
  [@@deriving sexp_of, variants]

  let refresh = Get
end

let apply_action (model : Model.t) (action : Action.t) (state : State.t) ~schedule_action =
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
        let open Date in
        let open Bookings in
        arrival <= model.date && departure >= model.date
      in
      read ~filter Bookings.t
    in
    Xhr.send' ~c ~handler request;
    { model with bookings = `Loading }
  | Date date ->
    schedule_action Action.Get;
    { model with date }
  | Next_day ->
    schedule_action Action.Get;
    init { model with date = Date.add_days model.date 1 }
  | Prev_day ->
    schedule_action Action.Get;
    init { model with date = Date.add_days model.date (-1) }
;;

open Vdom
open Incr.Let_syntax
open Ghm

let card ~rooms (booking : Pg.Bookings.return) =
  let rooms = Booking.Rooms.to_string rooms
  and n_guests = List.length booking.data.guests
  and till = Localize.date (Period.till booking.data.period)
  and link =
    Nav.href (Customer (Id booking.customer.id, Booking (Id booking.id, BData)))
  in
  let open Node in
  let open Attr in
  div
    [ classes [ "card"; "mb-2" ] ]
    [ div [ class_ "card-header" ] [ text booking.customer.keyword ]
    ; div
        [ class_ "card-body" ]
        [ p [] [ text (sprintf "Zimmer: %s" rooms) ]
        ; p [] [ text (sprintf "Gäste: %i" n_guests) ]
        ; p [] [ text (sprintf "Abreise: %s" till) ]
        ; a [ href link; class_ "stretched-link" ] [ text "Details anzeigen" ]
        ]
    ]
;;

let agenda ~date ~bookings =
  let open Pg.Bookings in
  let leave, arrive, stay =
    List.fold bookings ~init:([], [], []) ~f:(fun (l, a, s) b ->
        if Period.from b.data.period = date
        then l, b :: a, s
        else if Period.till b.data.period = date
        then b :: l, a, s
        else l, a, b :: s)
  in
  let cards head bookings =
    let bookings_by_rooms =
      List.map ~f:(fun b -> Booking.Rooms.of_booking b.data, b) bookings
    in
    let bookings_by_rooms =
      let compare a b = Booking.Rooms.compare (fst a) (fst b) in
      List.sort bookings_by_rooms ~compare
    in
    Node.h4 [ Attr.style Css_gen.(text_align `Center) ] [ Node.text head ]
    :: Node.hr []
    :: List.map ~f:(fun (rooms, b) -> card ~rooms b) bookings_by_rooms
  in
  let open Bs.Grid in
  row
    [ col4 (cards "Abreise" leave)
    ; col4 (cards "Bleiben" stay)
    ; col4 (cards "Anreise" arrive)
    ]
;;

let datepicker ~inject ~init =
  let interactive =
    let prepend =
      [ Bs.button
          (Icon (S "step-backward", "Vorheriger Tag"))
          (Action (fun _ -> inject Action.prev_day))
      ]
    and append =
      [ Bs.button
          (Icon (S "step-forward", "Nächster Tag"))
          (Action (fun _ -> inject Action.next_day))
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
  let%map agenda =
    let%bind date = model >>| Model.date in
    model
    >>| Model.bookings
    >>| function
    | `Ok bookings -> agenda ~date ~bookings
    | _ -> Bs.Grid.loading_row
  and datepicker =
    let%bind init = model >>| Model.date_init in
    datepicker ~inject ~init
  in
  Node.div [] [ datepicker; agenda ]
;;

let create ~inject model =
  let%map model = model
  and view = view ~inject model in
  let apply_action = apply_action model in
  Component.create ~apply_action model view
;;
