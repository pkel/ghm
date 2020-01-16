open Core_kernel
open Ghm
open Incr_dom
open Incr.Let_syntax

type env =
  { customer : Customer.t Incr.t
  ; customer_id : Nav.noi Incr.t
  }

module Model = struct
  type t =
    { remote : Pg.Bookings.return option
    ; booking : Booking_form.Model.t
    ; invoice : Invoice_form.Model.t
    ; last_valid : Booking.t
    ; last_input_at : int (* TODO: make nav read only incremental argument *)
    ; nav : Nav.noi * Nav.booking
    ; is_loading : bool
    ; is_valid : bool
    }
  [@@deriving compare, fields]

  let fresh_booking () =
    let from = Ext_date.today () in
    let till = Date.add_days from 7 in
    let period = Period.of_dates from till in
    Booking.empty ~period
  ;;

  let booking_id { nav; _ } = fst nav

  let t ?(is_loading = false) ?(b = fresh_booking ()) nav =
    { remote = None
    ; booking = Booking_form.init b
    ; invoice = Invoice_form.init (Option.value ~default:Invoice.empty b.invoice)
    ; last_valid = b
    ; is_valid = true
    ; last_input_at = Int.max_value
    ; nav
    ; is_loading
    }
  ;;

  let loading nav = t ~is_loading:true nav

  let loaded (remote : Pg.Bookings.return) nav =
    let m = t ~is_loading:false ~b:remote.data nav in
    { m with remote = Some remote }
  ;;

  let create () = t (Nav.New, Nav.BData)

  let sync m =
    if m.is_valid
    then (
      match m.remote with
      | None -> `Async
      | Some remote ->
        if Booking.compare m.last_valid remote.data = 0 then `Sync else `Async)
    else `Invalid_input
  ;;
end

module Action = struct
  type t =
    | Booking of Booking_form.Action.t
    | Invoice of Invoice_form.Action.t
    | Delayed_after_input
    | Delete
    | Save
    | Reload_invoice
    | Pg_posted of Pg.Bookings.return sexp_opaque Or_error.t
    | Pg_patched of Pg.Bookings.return sexp_opaque Or_error.t
    | Pg_got of Pg.Bookings.return sexp_opaque Or_error.t
    | NavChange of (Nav.noi * Nav.booking) sexp_opaque
  [@@deriving sexp_of, variants]
end

let apply_action
    ~customer
    ~customer_id
    ~booking
    ~invoice
    (model : Model.t)
    (action : Action.t)
    (state : State.t)
    ~schedule_action
    : Model.t
  =
  let delay_after_input model =
    (* Form touched. Wait a bit. Avoid high frequent saving. *)
    let () =
      Async_kernel.(
        don't_wait_for
          (after (Time_ns.Span.of_sec 0.3)
          >>| fun () -> schedule_action Action.Delayed_after_input))
    in
    Model.{ model with last_input_at = Browser.Date.(to_int (now ())) }
  in
  match action with
  | Booking action ->
    let schedule_action = Fn.compose schedule_action Action.booking in
    let booking = Component.apply_action ~schedule_action booking action state in
    let is_valid, last_valid =
      match Booking_form.eval booking with
      | Ok x -> true, { x with invoice = model.last_valid.invoice }
      | _ -> false, model.last_valid
    in
    delay_after_input { model with booking; last_valid; is_valid }
  | Invoice action ->
    let schedule_action = Fn.compose schedule_action Action.invoice in
    let invoice = Component.apply_action ~schedule_action invoice action state in
    let is_valid, last_valid =
      match Invoice_form.eval invoice with
      | Ok i ->
        ( true
        , let x = model.last_valid in
          { x with invoice = Some i } )
      | _ -> false, model.last_valid
    in
    delay_after_input { model with invoice; last_valid; is_valid }
  | Delayed_after_input ->
    let () =
      if Browser.Date.(to_int (now ())) - model.last_input_at >= 300
      then schedule_action Action.Save
      else ()
    in
    model
  | Save ->
    let () =
      match customer_id with
      | Nav.New -> ()
      | Id customer ->
        if Some model.last_valid <> Option.map ~f:(fun x -> x.data) model.remote
        then (
          let body = { Pg.Bookings.data = model.last_valid; customer } in
          let c = state.connection in
          match model.remote with
          | None ->
            let handler = Fn.compose schedule_action Action.pg_posted in
            Xhr.send ~c ~body ~handler Pg.(create Bookings.t)
          | Some remote ->
            let handler = Fn.compose schedule_action Action.pg_patched in
            Xhr.send
              ~c
              ~body
              ~handler
              Pg.(update' Int.(Bookings.id' == remote.id) Bookings.t))
    in
    model
  | Reload_invoice ->
    let invoice = Invoice_gen.gen ~date:(Ext_date.today ()) customer model.last_valid in
    let invoice = Invoice_form.init invoice
    and last_valid =
      let x = model.last_valid in
      { x with invoice = Some invoice }
    in
    delay_after_input { model with invoice; last_valid }
  | Pg_patched (Ok remote) -> { model with remote = Some remote }
  | Pg_posted (Ok remote) ->
    let nav = Nav.Id remote.id, snd model.nav in
    let () = Nav.set (Nav.Customer (customer_id, Nav.Booking nav)) in
    { model with remote = Some remote; nav }
  | Pg_got (Ok return) -> Model.loaded return model.nav
  | Pg_posted (Error detail) | Pg_patched (Error detail) ->
    state.handle_error { gist = "Speichern fehlgeschlagen"; detail };
    model
  | Pg_got (Error detail) ->
    state.handle_error { gist = "Laden fehlgeschlagen"; detail };
    model
  | Delete ->
    let () =
      match model.nav with
      | New, _ -> Nav.set (Nav.Customer (customer_id, Nav.CData))
      | Id i, _ ->
        let handler = function
          | Error detail ->
            state.handle_error { gist = "Löschen fehlgeschlagen"; detail }
          | Ok () -> Nav.set (Nav.Customer (customer_id, Nav.CData))
        in
        let c = state.connection in
        Xhr.send' ~c ~handler Pg.(delete Int.(Bookings.id = i) Bookings.t)
    in
    (* This hack makes the parent component reload. *)
    { model with nav = Nav.(Id Int.min_value, BData) }
  | NavChange nav ->
    if model.nav <> nav
    then (
      match fst nav with
      | Nav.New -> Model.t nav
      | Id i ->
        let rq = Pg.(read' Int.(Bookings.id' == i) Bookings.t) in
        let handler = Fn.compose schedule_action Action.pg_got in
        let c = state.connection in
        Xhr.send' ~c ~handler rq;
        Model.loading nav)
    else { model with nav }
;;

let danger_btn action title =
  Bs.button ~tabskip:true ~color:`Outline_danger (Text title) (Action action)
;;

let save_btn ~sync ~inject =
  let action _ = inject Action.(Save) in
  match sync with
  | `Sync ->
    Bs.button ~color:`Outline_success (Icon (S "check", "Gespeichert")) (Action action)
  | `Async ->
    Bs.button ~color:`Outline_warning (Icon (S "save", "Speichern")) (Action action)
  | `Invalid_input ->
    Bs.button ~color:`Outline_danger (Icon (S "times", "Speichern")) (Action action)
;;

let view_booking ~sync ~inject ~form ~customer ~booking =
  let delete_c _evt = inject Action.Delete in
  let%map sync = sync
  and form = form
  and confirmation, excel =
    let%map booking = booking
    and customer = customer in
    let date = Browser.Date.(now () |> to_locale_date_string) in
    ( Letter.(confirm ~booking ~date customer |> href)
    , Excel_br_2014_v2.of_customer_and_booking customer booking )
  in
  Bs.Grid.
    [ Component.view form
    ; frow
        ~c:[ "mt-5"; "pb-3" ]
        [ col_auto [ Bs.button (Text "Bestätigung") (Href_blank confirmation) ]
        ; col_auto [ Bs.button (Text "Excel") (Clipboard excel) ]
        ; col
            [ frow
                ~c:[ "justify-content-end" ]
                [ col_auto [ danger_btn delete_c "Buchung löschen" ]
                ; col_auto [ save_btn ~sync ~inject ]
                ]
            ]
        ]
    ]
;;

let view_invoice ~sync ~inject ~form ~booking =
  let reload_c _evt = inject Action.Reload_invoice in
  let%map sync = sync
  and form = form
  and confirmation =
    let%map x = booking >>| Booking.invoice >>| Option.value ~default:Invoice.empty in
    Letter.(invoice x |> href)
  in
  Bs.Grid.
    [ Component.view form
    ; frow
        ~c:[ "mt-5"; "pb-3" ]
        [ col_auto [ Bs.button (Text "Drucken") (Href_blank confirmation) ]
        ; col
            [ frow
                ~c:[ "justify-content-end" ]
                [ col_auto [ danger_btn reload_c "Neu Laden" ]
                ; col_auto [ save_btn ~sync ~inject ]
                ]
            ]
        ]
    ]
;;

let view ~invoice ~booking (model : Model.t Incr.t) ~inject ~customer =
  let open Vdom in
  let sync = model >>| Model.sync
  and last_valid = model >>| Model.last_valid in
  let%bind booking =
    view_booking ~form:booking ~sync ~inject ~customer ~booking:last_valid
  and invoice = view_invoice ~sync ~form:invoice ~inject ~booking:last_valid in
  let%map nav = model >>| Model.nav in
  match nav with
  | _, BData -> Node.create "form" [] booking
  | _, Invoice -> Node.create "form" [] invoice
;;

let view ~customer ~inject ~booking ~invoice model =
  let cond = model >>| Model.is_loading in
  Incr.if_
    cond
    ~then_:(Incr.const Bs.Grid.loading_row)
    ~else_:(view ~customer ~booking ~invoice ~inject model)
;;

let menu ~customer_id (m : Model.t) : Menu.t =
  let open Menu in
  let abs rel = Nav.Customer (customer_id, Booking (fst m.nav, rel)) in
  let entry name rel =
    let href = Href (abs rel |> Nav.href)
    and active = snd m.nav = rel in
    entry name href active
  in
  [ entry "Buchungsdaten" BData; entry "Rechnung" Invoice ]
;;

let create ~(env : env) ~(inject : Action.t -> Vdom.Event.t) (model : Model.t Incr.t) =
  let booking =
    let inject = Fn.compose inject Action.booking in
    Booking_form.create ~env:() ~inject (model >>| Model.booking)
  and invoice =
    let inject = Fn.compose inject Action.invoice in
    Invoice_form.create ~env:() ~inject (model >>| Model.invoice)
  in
  let%map model = model
  and customer_id = env.customer_id
  and customer = env.customer
  and view =
    let customer = env.customer in
    view ~customer ~inject ~booking ~invoice model
  and booking = booking
  and invoice = invoice in
  let apply_action = apply_action ~customer ~booking ~invoice ~customer_id model
  and extra : Menu.t * Period.t option =
    menu ~customer_id model, Option.map model.remote ~f:(fun x -> x.data.period)
  in
  Component.create_with_extra ~apply_action ~extra model view
;;
