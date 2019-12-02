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
    ; booking : Booking_form.Model.t (* ; invoice : Invoice_form.Model.t *)
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

  let t
      ?(nav = Nav.(New, BData))
      ?(is_loading = false)
      ?(b = { Pg.Bookings.id = -1; data = fresh_booking (); customer = -1 })
      ()
    =
    { remote = b
    ; booking = Booking_form.init b.data
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
    (* | Invoice of Invoice_form.Action.t *)
    | Delayed_after_input
    | Delete
    | Save
    | Pg_posted of Pg.Bookings.return sexp_opaque Or_error.t
    | Pg_patched of Pg.Bookings.return sexp_opaque Or_error.t
    | Pg_got of Pg.Bookings.return sexp_opaque Or_error.t
    | NavChange of (Nav.noi * Nav.booking) sexp_opaque
  [@@deriving sexp_of, variants]
end

let apply_action
    ~customer_id
    ~booking
    (* ~invoice *)
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
  (* | Invoice action ->
    let schedule_action = Fn.compose schedule_action Action.invoice in
    let invoice = Component.apply_action ~schedule_action invoice action state in
    let is_valid, last_valid =
      match Invoice_form.eval invoice with
      | Ok invoice -> true, let x = model.last_valid in { x with invoice }
      | _ -> false, model.last_valid
    in
    delay_after_input { model with invoice; last_valid; is_valid } *)
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
      | New, _ -> Nav.(set Overview)
      | Id i, _ ->
        let handler = function
          | Error detail ->
            state.handle_error { gist = "Löschen fehlgeschlagen"; detail }
          | Ok () -> Nav.(set Overview)
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

open Vdom

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

let uri_of_letter customer t =
  let date = Browser.Date.(now () |> to_locale_date_string) in
  Letter.(t ~date customer |> href)
;;

let letter_dropdown_id = "letter_dropdown_menu"

let letter_dropdown customer =
  let items =
    let f x =
      let uri = uri_of_letter customer in
      Node.a
        Attr.[ class_ "dropdown-item"; href (uri (snd x)); create "target" "_blank" ]
        [ Node.text (fst x) ]
    and letters =
      let open Letter in
      [ "Leere Vorlage", blank ~attachments:[]
      ; "Leere Vorlage (mit Anhang)", blank ~attachments:[ "Anhang 1"; "Anhang 2" ]
      ; "Hausprospekt", flyer
      ]
    in
    List.map ~f letters
  in
  Node.(
    div
      [ Attr.classes [ "dropdown"; "dropup" ] ]
      [ a
          Attr.
            [ classes [ "btn"; "btn-secondary"; "dropdown-toggle" ]
            ; href "#"
            ; type_ "button"
            ; id letter_dropdown_id
            ; create "data-toggle" "dropdown"
            ; create "aria-haspopup" "true"
            ; create "aria-expanded" "false"
            ]
          [ Node.text "Brief" ]
      ; div
          Attr.[ class_ "dropdown-menu"; create "aria-labelledby" letter_dropdown_id ]
          items
      ])
;;

let view ~sync ~inject ~form ~customer =
  let delete_c _evt = inject Action.Delete in
  let%map letter_dropdown = customer >>| letter_dropdown
  and sync = sync
  and form = form in
  Bs.Grid.(
    [ Node.h4 [] [ Node.text "Stammdaten" ]; Node.hr []; Component.view form ]
    @ [ frow
          [ col_auto ~c:[ "mb-2"; "mt-2" ] [ letter_dropdown ]
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
      ])
;;

let view ~form (model : Model.t Incr.t) ~inject ~customer =
  let open Vdom in
  let%map form =
    let sync = model >>| Model.sync in
    view ~form ~sync ~inject ~customer
  in
  let save _evt = inject Action.(Save) in
  Node.create "form" [ Attr.on "submit" save ] form
;;

let view ~customer ~inject ~form model =
  let cond = model >>| Model.is_loading in
  Incr.if_
    cond
    ~then_:(Incr.const Bs.Grid.loading_row)
    ~else_:(view ~customer ~form ~inject model)
;;

let menu (_m : Model.t) : Menu.t = []

let create ~(env : env) ~(inject : Action.t -> Vdom.Event.t) (model : Model.t Incr.t) =
  let booking =
    let inject = Fn.compose inject Action.booking in
    Booking_form.create ~env:() ~inject (model >>| Model.booking)
  in
  let%map model = model
  and customer_id = env.customer_id
  and view =
    let customer = env.customer
    and form = booking in
    view ~customer ~inject ~form model
  and booking = booking in
  let apply_action = apply_action ~booking ~customer_id model
  and extra : Menu.t = menu model in
  Component.create_with_extra ~apply_action ~extra model view
;;
