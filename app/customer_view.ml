open Core_kernel
open Ghm
open Incr_dom
open Incr.Let_syntax

module Model = struct
  type t =
    { remote : Pg.Customers.return option
    ; form : Customer_form.Model.t
    ; booking : Booking_view.Model.t
    ; last_valid : Customer.t
    ; last_input_at : int
    ; nav : Nav.noi * Nav.customer
    ; is_loading : bool
    ; is_saving : bool (* save mutex / avoid concurrent HTTP PATCH/POST *)
    ; is_valid : bool
    }
  [@@deriving compare, fields]

  let t
      ?(old_booking = Booking_view.Model.create ())
      ?(is_loading = false)
      ?(c = Customer.empty)
      nav
    =
    { remote = None
    ; form = Customer_form.init c
    ; booking = old_booking
    ; last_valid = c
    ; is_valid = true
    ; last_input_at = Int.max_value
    ; nav
    ; is_loading
    ; is_saving = false
    }
  ;;

  let loading nav = t ~is_loading:true nav

  let loaded m (remote : Pg.Customers.return) =
    let m = t ~old_booking:m.booking ~is_loading:false ~c:remote.data m.nav in
    { m with remote = Some remote }
  ;;

  let create () = t Nav.(New, CData)

  let sync m =
    if m.is_saving
    then `Saving
    else if m.is_valid
    then (
      match m.remote with
      | None -> `Async
      | Some remote ->
        if Customer.compare m.last_valid remote.data = 0 then `Sync else `Async)
    else `Invalid_input
  ;;
end

module Action = struct
  type t =
    | Form of Customer_form.Action.t
    | Booking of Booking_view.Action.t
    | Delayed_after_input
    | DeleteCustomer
    | Save
    | PostedCustomer of Pg.Customers.return sexp_opaque Or_error.t
    | PatchedCustomer of Pg.Customers.return sexp_opaque Or_error.t
    | NavChange of (Nav.noi * Nav.customer) sexp_opaque
    | GotCustomerRefresh of Pg.Customers.return sexp_opaque Or_error.t
    | GotCustomerLoad of Pg.Customers.return sexp_opaque Or_error.t
  [@@deriving sexp_of, variants]
end

let apply_action
    ~form
    ~booking
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
  | Form action ->
    let schedule_action = Fn.compose schedule_action Action.form in
    let form = Component.apply_action ~schedule_action form action state in
    let is_valid, last_valid =
      match Customer_form.eval form with
      | Ok x -> true, x
      | _ -> false, model.last_valid
    in
    delay_after_input { model with form; last_valid; is_valid }
  | Booking action ->
    let booking =
      let schedule_action = Fn.compose schedule_action Action.booking in
      Component.apply_action ~schedule_action booking action state
    in
    let () =
      let open Booking_view.Model in
      (* This indicates that a new booking was created, or the booking was deleted. *)
      if booking_id model.booking <> booking_id booking
      then (
        match fst model.nav with
        | Nav.New -> ()
        | Id i ->
          let rq = Pg.(read' Int.(Customers.id' == i) Customers.t) in
          let handler = Fn.compose schedule_action Action.gotcustomerrefresh in
          let c = state.connection in
          Async_kernel.(
            don't_wait_for
              (after (Time_ns.Span.of_sec 0.3) >>| fun () -> Xhr.send' ~c ~handler rq)))
    in
    { model with booking }
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
    else if Some model.last_valid <> Option.map ~f:(fun x -> x.data) model.remote
    then (
      let () =
        let body = model.last_valid in
        let c = state.connection in
        match model.remote with
        | None ->
          let handler = Fn.compose schedule_action Action.postedcustomer in
          Xhr.send ~c ~body ~handler Pg.(create Customers.t)
        | Some remote ->
          let handler = Fn.compose schedule_action Action.patchedcustomer in
          Xhr.send
            ~c
            ~body
            ~handler
            Pg.(update' Int.(Customers.id' == remote.id) Customers.t)
      in
      { model with is_saving = true })
    else model
  | GotCustomerLoad (Ok return) -> Model.loaded model return
  | GotCustomerRefresh (Error detail) | GotCustomerLoad (Error detail) ->
    state.handle_error { gist = "Laden fehlgeschlagen"; detail };
    model
  | GotCustomerRefresh (Ok x) -> { model with remote = Some x }
  | PatchedCustomer (Ok x) -> { model with remote = Some x; is_saving = false }
  | PostedCustomer (Ok remote) ->
    let nav = Nav.Id remote.id, snd model.nav in
    let () = Nav.set (Customer nav) in
    { model with remote = Some remote; nav; is_saving = false }
  | PostedCustomer (Error detail) | PatchedCustomer (Error detail) ->
    state.handle_error { gist = "Speichern fehlgeschlagen"; detail };
    { model with is_saving = false }
  | DeleteCustomer ->
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
        Xhr.send' ~c ~handler Pg.(delete Int.(Customers.id = i) Customers.t)
    in
    model
  | NavChange nav ->
    let () =
      match snd nav with
      | Booking bnav -> schedule_action (Booking (Booking_view.Action.navchange bnav))
      | CData -> ()
    in
    if fst model.nav <> fst nav
    then (
      match fst nav with
      | Nav.New -> Model.t nav
      | Id i ->
        let rq = Pg.(read' Int.(Customers.id' == i) Customers.t) in
        let handler = Fn.compose schedule_action Action.gotcustomerload in
        let c = state.connection in
        Xhr.send' ~c ~handler rq;
        Model.loading nav)
    else { model with nav }
;;

open Vdom

let danger_btn action title =
  Bs.button ~color:`Outline_danger ~tabskip:true (Text title) (Action action)
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

let view ~sync ~inject ~form customer =
  let delete_c _evt = inject Action.DeleteCustomer in
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
                      [ danger_btn delete_c "Kunde löschen" ]
                  ; col_auto ~c:[ "mb-2"; "mt-2" ] [ save_btn ~sync ~inject ]
                  ]
              ]
          ]
      ])
;;

let view ~form (model : Model.t Incr.t) ~inject =
  let open Vdom in
  let%map form =
    let sync = model >>| Model.sync in
    view ~form ~sync ~inject (model >>| Model.last_valid)
  in
  Node.create "form" [] form
;;

let view ~inject ~form model =
  let cond = model >>| Model.is_loading in
  Incr.if_ cond ~then_:(Incr.const Bs.Grid.loading_row) ~else_:(view ~form ~inject model)
;;

let view ~inject ~form ~booking model =
  match%bind model >>| Model.nav >>| snd with
  | CData -> view ~inject ~form model
  | Booking _ ->
    let%map booking = booking in
    Component.view booking
;;

(* TODO: this feels super clumsy. *)
let menu ~booking (m : Model.t) : Menu.t =
  let open Menu in
  let open Nav in
  let abs rel = Customer (fst m.nav, rel) in
  let entry ?children name rel =
    let href = Href (abs rel |> Nav.href)
    and active = snd m.nav = rel in
    entry ?children name href active
  in
  let bookings =
    Option.map m.remote ~f:(fun x -> x.bookings) |> Option.value ~default:[]
  in
  let children =
    entry "Stammdaten" CData
    ::
    (let children = [] in
     entry ~children "Neue Buchung" (Booking New)
     :: List.map bookings ~f:(fun { arrival; departure; id } ->
            let period =
              let fb = Period.of_dates arrival departure in
              match snd m.nav with
              | Booking (Id id') when id' = id -> Option.value ~default:fb booking
              | _ -> fb
            in
            entry ~children Period.(to_string_hum period) (Booking (Id id))))
  in
  let title =
    if m.is_loading
    then "Kunde lädt ..."
    else (
      match String.strip m.last_valid.keyword with
      | "" -> "Kunde: n/a"
      | s -> "Kunde: " ^ s)
  in
  [ entry ~children title CData ]
;;

let create ~(inject : Action.t -> Vdom.Event.t) (model : Model.t Incr.t) =
  let form =
    let inject = Fn.compose inject Action.form in
    Customer_form.create ~env:() ~inject (model >>| Model.form)
  and booking =
    let inject = Fn.compose inject Action.booking
    and env =
      { Booking_view.customer = model >>| Model.last_valid
      ; customer_id = model >>| Model.nav >>| fst
      }
    in
    Booking_view.create ~env ~inject (model >>| Model.booking)
  in
  let%map model = model
  and view = view ~inject ~booking ~form model
  and form = form
  and booking = booking in
  let apply_action = apply_action ~booking ~form model
  and extra : Menu.t = menu ~booking:(Component.extra booking) model in
  Component.create_with_extra ~apply_action ~extra model view
;;
