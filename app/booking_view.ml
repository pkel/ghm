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
    { remote : Pg.Bookings.return
    ; booking : Booking_form.Model.t
    ; invoice : Invoice_form.Model.t
    ; last_valid : Booking.t
    ; last_input_at : int
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

  let dummy_return () =
    let customer : Pg.Bookings.return_customer = { id = -1; keyword = "n/a" } in
    { Pg.Bookings.id = -1; data = fresh_booking (); customer }
  ;;

  let t ?(nav = Nav.(New, BData)) ?(is_loading = false) ?(b = dummy_return ()) () =
    { remote = b
    ; booking = Booking_form.init b.data
    ; invoice = Invoice_form.init (Option.value ~default:Invoice.empty b.data.invoice)
    ; last_valid = b.data
    ; is_valid = true
    ; last_input_at = Int.max_value
    ; nav
    ; is_loading
    }
  ;;

  let loading nav = t ~is_loading:true ~nav ()
  let loaded m b = t ~nav:m.nav ~is_loading:false ~b ()
  let create () = t ()

  let sync m =
    if m.is_valid
    then if Booking.compare m.last_valid m.remote.data = 0 then `Sync else `Async
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
        let body = { Pg.Bookings.data = model.last_valid; customer } in
        if body.data <> model.remote.data
        then (
          let c = state.connection in
          if model.remote.id < 0
          then (
            let handler = Fn.compose schedule_action Action.pg_posted in
            Xhr.send ~c ~body ~handler Pg.(create Bookings.t))
          else (
            let handler = Fn.compose schedule_action Action.pg_patched in
            Xhr.send
              ~c
              ~body
              ~handler
              Pg.(update' Int.(Bookings.id' == model.remote.id) Bookings.t)))
    in
    model
  | Reload_invoice ->
    let invoice =
      Invoice_form.init
        (Invoice_gen.gen ~date:(Ext_date.today ()) customer model.last_valid)
    in
    { model with invoice }
  | Pg_patched (Ok remote) -> { model with remote }
  | Pg_posted (Ok remote) ->
    let nav = Nav.Id remote.id, snd model.nav in
    let () = Nav.set (Nav.Customer (customer_id, Nav.Booking nav)) in
    { model with remote; nav }
  | Pg_got (Ok return) -> Model.loaded model return
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
    model
  | NavChange (New, _) -> Model.create ()
  | NavChange ((Id i, _) as nav) when Nav.Id i <> fst model.nav ->
    let rq = Pg.(read' Int.(Bookings.id' == i) Bookings.t) in
    let handler = Fn.compose schedule_action Action.pg_got in
    let c = state.connection in
    Xhr.send' ~c ~handler rq;
    Model.loading nav
  | NavChange nav -> { model with nav }
;;

let danger_btn action title =
  Bs.button ~attr:[ Bs.tab_skip ] ~style:"outline-danger" ~action title
;;

let save_btn ~sync ~inject =
  let action _ = inject Action.(Save) in
  match sync with
  | `Sync -> Bs.button ~action ~i:(S "check") ~style:"outline-success" "Gespeichert"
  | `Async -> Bs.button ~action ~i:(S "save") ~style:"outline-warning" "Speichern"
  | `Invalid_input -> Bs.button ~action ~i:(S "times") ~style:"outline-danger" "Speichern"
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
        [ col_auto
            ~c:[ "mb-2"; "mt-2" ]
            [ Bs.button' ~href:confirmation ~blank:true "Bestätigung" ]
        ; col_auto ~c:[ "mb-2"; "mt-2" ] [ Bs.button_clipboard ~value:excel "Excel" ]
        ; col
            [ frow
                ~c:[ "justify-content-end" ]
                [ col_auto
                    ~c:[ "mb-2"; "mt-2" ]
                    [ danger_btn delete_c "Buchung löschen" ]
                ; col_auto ~c:[ "mb-2"; "mt-2" ] [ save_btn ~sync ~inject ]
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
        [ col_auto
            ~c:[ "mb-2"; "mt-2" ]
            [ Bs.button' ~href:confirmation ~blank:true "Drucken" ]
        ; col
            [ frow
                ~c:[ "justify-content-end" ]
                [ col_auto ~c:[ "mb-2"; "mt-2" ] [ danger_btn reload_c "Neu Laden" ]
                ; col_auto ~c:[ "mb-2"; "mt-2" ] [ save_btn ~sync ~inject ]
                ]
            ]
        ]
    ]
;;

let view ~invoice ~booking (model : Model.t Incr.t) ~inject ~customer =
  let open Vdom in
  let _ = invoice in
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
  and extra : Menu.t = menu ~customer_id model in
  Component.create_with_extra ~apply_action ~extra model view
;;
