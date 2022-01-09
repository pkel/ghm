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
    ; nav : Nav.noi
    ; is_loading : bool
    ; is_saving : bool (* save mutex / avoid concurrent HTTP PATCH/POST *)
    ; is_valid : bool
    }
  [@@deriving compare, fields]

  let default_period () =
    let from = Ext_date.today () in
    let till = Date.add_days from 7 in
    Period.of_dates from till
  ;;

  let fresh_booking () =
    let period = default_period () in
    Booking.empty ~period
  ;;

  let booking_id { nav; _ } = nav

  let t ?(is_loading = false) ?(b = fresh_booking ()) nav =
    { remote = None
    ; booking = Booking_form.init b
    ; invoice = Invoice_form.init (Option.value ~default:Invoice.empty b.invoice)
    ; last_valid = b
    ; is_valid = true
    ; last_input_at = Int.max_value
    ; nav
    ; is_loading
    ; is_saving = false
    }
  ;;

  let loading nav = t ~is_loading:true nav

  let loaded (remote : Pg.Bookings.return) nav =
    let m = t ~is_loading:false ~b:remote.data nav in
    { m with remote = Some remote }
  ;;

  let create () = t Nav.New

  let sync m =
    if m.is_saving
    then `Saving
    else if m.is_valid
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
    | Create_invoice
    | Delete_invoice
    | Pg_posted of Pg.Bookings.return sexp_opaque Or_error.t
    | Pg_patched of Pg.Bookings.return sexp_opaque Or_error.t
    | Pg_got of Pg.Bookings.return sexp_opaque Or_error.t
    | NavChange of Nav.noi sexp_opaque
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
    if model.is_saving
    then (* avoid concurrent saves, especially posts *)
      delay_after_input model
    else (
      match customer_id with
      | Nav.Id customer
        when Some model.last_valid <> Option.map ~f:(fun x -> x.data) model.remote ->
        let body = { Pg.Bookings.data = model.last_valid; customer } in
        let c = state.connection in
        let () =
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
              Pg.(update' Int.(Bookings.id' == remote.id) Bookings.t)
        in
        { model with is_saving = true }
      | Nav.New | _ -> model)
  | Create_invoice ->
    let invoice = Invoice_gen.gen ~date:(Ext_date.today ()) customer model.last_valid in
    let invoice = Invoice_form.init invoice
    and last_valid =
      let x = model.last_valid in
      { x with invoice = Some invoice }
    in
    delay_after_input { model with invoice; last_valid }
  | Delete_invoice ->
    let last_valid = { model.last_valid with invoice = None } in
    delay_after_input { model with last_valid }
  | Pg_patched (Ok remote) -> { model with remote = Some remote; is_saving = false }
  | Pg_posted (Ok remote) ->
    let nav = Nav.Id remote.id in
    let () = Nav.set (Nav.Customer (customer_id, Nav.Booking nav)) in
    { model with remote = Some remote; nav; is_saving = false }
  | Pg_got (Ok return) -> Model.loaded return model.nav
  | Pg_posted (Error detail) | Pg_patched (Error detail) ->
    state.handle_error { gist = "Speichern fehlgeschlagen"; detail };
    { model with is_saving = false }
  | Pg_got (Error detail) ->
    state.handle_error { gist = "Laden fehlgeschlagen"; detail };
    model
  | Delete ->
    let () =
      match model.nav with
      | New -> Nav.set (Nav.Customer (customer_id, Nav.CData))
      | Id i ->
        let handler = function
          | Error detail ->
            state.handle_error { gist = "Löschen fehlgeschlagen"; detail }
          | Ok () -> Nav.set (Nav.Customer (customer_id, Nav.CData))
        in
        let c = state.connection in
        Xhr.send' ~c ~handler Pg.(delete Int.(Bookings.id = i) Bookings.t)
    in
    (* This hack makes the parent component reload. *)
    { model with nav = Nav.(Id Int.min_value) }
  | NavChange nav ->
    if model.nav <> nav
    then (
      match nav with
      | Nav.New ->
        let b =
          let current = model.last_valid in
          { current with
            allocs = []
          ; deposit_asked = None
          ; deposit_got = None
          ; note = ""
          ; period = Model.default_period ()
          ; invoice = None
          }
        in
        Model.t ~b nav
      | Id id ->
        let rq = Pg.(read' Int.(Bookings.id' == id) Bookings.t) in
        let handler = Fn.compose schedule_action Action.pg_got in
        let c = state.connection in
        Xhr.send' ~c ~handler rq;
        Model.loading nav)
    else model
;;

let danger_btn ?disabled action title =
  Bs.button ?disabled ~tabskip:true ~color:`Outline_danger (Text title) (Action action)
;;

let save_btn ~sync ~inject =
  let action _ = inject Action.(Save) in
  match sync with
  | `Sync ->
    Bs.button ~color:`Outline_success (Icon (S "check", "Gespeichert")) (Action action)
  | `Async ->
    Bs.button ~color:`Outline_warning (Icon (S "save", "Speichern")) (Action action)
  | `Saving ->
    Bs.button
      ~color:`Outline_warning
      (Icon (S "hourglass", "Speichert ..."))
      (Action action)
  | `Invalid_input ->
    Bs.button ~color:`Outline_danger (Icon (S "times", "Speichern")) (Action action)
;;

let view_booking ~sync ~inject ~main_form ~invoice_form ~customer ~booking =
  let delete_booking_c _evt = inject Action.Delete
  and create_invoice_c _evt = inject Action.Create_invoice
  and delete_invoice_c _evt = inject Action.Delete_invoice in
  let%map sync = sync
  and main_form = main_form
  and invoice_form = invoice_form
  and lock_invoice = booking >>| Booking.invoice >>| Option.is_some
  and confirmation, excel, meldeschein =
    let%map booking = booking
    and customer = customer in
    let filename =
      let open Browser.Date in
      let now = now () in
      let year = get_full_year now
      and month = get_month now
      and day = get_date now in
      sprintf
        "Meldeschein - %04i%02i%02i - %s.xml"
        year
        month
        day
        Customer.(customer.keyword)
    and content = Ghm.Meldeschein.xml customer booking in
    let date = Browser.Date.(now () |> to_locale_date_string) in
    ( Letter.(confirm ~booking ~date customer |> href)
    , Excel_br_2014_v2.of_customer_and_booking customer booking
    , Bs.Download { filename; media_type = "application/xml"; content } )
  and invoice =
    let%map x = booking >>| Booking.invoice >>| Option.value ~default:Invoice.empty in
    Letter.(invoice x |> href)
  in
  let open Bs.Grid in
  let buttons =
    frow
      ~c:[ "mt-3"; "mb-3" ]
      [ col_auto [ Bs.button (Text "Bestätigung") (Href_blank confirmation) ]
      ; col_auto [ Bs.button (Text "Excel") (Clipboard excel) ]
      ; col_auto [ Bs.button (Text "Meldeschein") meldeschein ]
      ; (if lock_invoice
        then col_auto [ Bs.button (Text "Rechnung drucken") (Href_blank invoice) ]
        else col_auto [ Bs.button (Text "Rechnung erstellen") (Action create_invoice_c) ])
      ; col
          [ frow
              ~c:[ "justify-content-end" ]
              [ col_auto
                  [ (if lock_invoice
                    then danger_btn delete_invoice_c "Rechnung zurücksetzen"
                    else danger_btn delete_booking_c "Buchung löschen")
                  ]
              ; col_auto [ save_btn ~sync ~inject ]
              ]
          ]
      ]
  and lock_warning =
    let open Vdom in
    frow
      ~c:[ "alert"; "alert-warning" ]
      [ col
          [ Node.text
              "Die Buchung kann nicht mehr bearbeitet werden, da bereits eine Rechnung \
               erstellt wurde. Um die Buchung zu bearbeiten, muss die Rechnung \
               zurückgesetzt werden."
          ]
      ]
  in
  List.filter_opt
    [ Some (Component.view main_form)
    ; (if lock_invoice then Some lock_warning else None)
    ; Some buttons
    ; (if lock_invoice then Some (Component.view invoice_form) else None)
    ]
;;

let view ~invoice ~booking (model : Model.t Incr.t) ~inject ~customer =
  let open Vdom in
  let sync = model >>| Model.sync
  and last_valid = model >>| Model.last_valid in
  view_booking
    ~main_form:booking
    ~invoice_form:invoice
    ~sync
    ~inject
    ~customer
    ~booking:last_valid
  >>| Node.div []
;;

let view ~customer ~inject ~booking ~invoice model =
  let cond = model >>| Model.is_loading in
  Incr.if_
    cond
    ~then_:(Incr.const Bs.Grid.loading_row)
    ~else_:(view ~customer ~booking ~invoice ~inject model)
;;

let create ~(env : env) ~(inject : Action.t -> Vdom.Event.t) (model : Model.t Incr.t) =
  let booking =
    let env =
      let lock_invoice =
        let%map invoice = model >>| Model.last_valid >>| Booking.invoice in
        if invoice = None then false else true
      in
      Booking_form.{ lock_invoice }
    and inject = Fn.compose inject Action.booking in
    Booking_form.create ~env ~inject (model >>| Model.booking)
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
  and extra : Period.t option = Option.map model.remote ~f:(fun x -> x.data.period) in
  Component.create_with_extra ~apply_action ~extra model view
;;
