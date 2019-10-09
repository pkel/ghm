(* TODO:
   - database interaction should NOT be handled here
   - local customer is exposed via extra, upper level should observe
     changes and handle database interaction
   - save booking

   - fix form (switch to booking then back to CData)
   - better: replace form with interactive

   - nav should not be part of the model
   - nav should be an incremental argument to the component
   - nav menu should be rendered externally

   - investigate effective use of Incremental for list of bookings

   - (done) there should be one invoice form per booking
   - (done) the exposed extra customer should integrate the exposed incremental
     invoices/bookings of the sub component
*)

open Core_kernel
open Ghm
open Incr_dom
open Incr_dom_widgets
open Incr.Let_syntax

module Model = struct
  type visit =
    | Fresh
    | Saved of Period.t
  [@@deriving compare]

  type t =
    { remote : Customer.t
    ; local : Customer.t
    ; bookings : (visit * Booking_form.Model.t) list
    ; nav : Nav.noi * Nav.customer
    ; touched_at : int
    ; sync : bool
    ; loading : bool
    }
  [@@deriving compare, fields]

  let load nav (c : Customer.t) =
    { remote = c
    ; local = c
    ; nav
    ; touched_at = Int.max_value
    ; bookings =
        List.map
          ~f:(fun b -> Saved (Booking.period b), Booking_form.Model.load b)
          c.bookings
    ; loading = false
    ; sync = true
    }
  ;;

  let create' ?(loading = false) ?(nav = Nav.(New, CData)) () =
    let m = load nav Customer.empty in
    { m with loading }
  ;;

  let create () = create' ()
end

module Action = struct
  type name =
    | Title of string
    | Letter of string
    | Given of string
    | Family of string
  [@@deriving sexp_of, variants]

  type address =
    | Street_with_num of string
    | Postal_code of string
    | City of string
    | Country of string
    | Country_code of string
  [@@deriving sexp_of, variants]

  type contact =
    | Phone of string
    | Phone2 of string
    | Mobile of string
    | Fax of string
    | Fax2 of string
    | Mail of string
    | Mail2 of string
    | Web of string
  [@@deriving sexp_of, variants]

  type company =
    | Company_name of string
    | Company_address of string
  [@@deriving sexp_of, variants]

  type customer =
    | Name of name
    | Company of company
    | Address of address
    | Contact of contact
    | Keyword of string
    | Note of string
  [@@deriving sexp_of, variants]

  type t =
    | Customer of customer
    | NavChange of (Nav.noi * Nav.customer) sexp_opaque
    | GotCustomer of (int * Customer.t) Or_error.t
    | PostedCustomer of (int * Customer.t) Or_error.t
    | PatchedCustomer of Customer.t Or_error.t
    | Save
    | Touch
    | Touched
    | NewBooking
    | DeleteBooking of int
    | DeleteCustomer
    | Booking of int * Booking_form.Action.t
  [@@deriving sexp_of, variants]
end

let default_period () =
  let today = Ext_date.today () in
  let from = Date.add_days today 7
  and till = Date.add_days today 8 in
  Period.of_dates from till
;;

let fresh_booking () =
  Booking.
    { deposit_asked = None
    ; deposit_got = None
    ; tax_free = false
    ; note = ""
    ; period = default_period ()
    ; guests = []
    ; allocs = []
    ; invoice = None
    }
;;

let apply_form_action x =
  let open Action in
  let open Customer in
  function
  | Name action ->
    let name =
      let x = x.name in
      match action with
      | Title title -> { x with title }
      | Letter letter -> { x with letter }
      | Given given -> { x with given }
      | Family family -> { x with family }
    in
    { x with name }
  | Address action ->
    let address =
      let x = x.address in
      match action with
      | Street_with_num street_with_num -> { x with street_with_num }
      | Postal_code postal_code -> { x with postal_code }
      | City city -> { x with city }
      | Country country -> { x with country }
      | Country_code country_code -> { x with country_code }
    in
    { x with address }
  | Contact action ->
    let contact =
      let x = x.contact in
      match action with
      | Phone phone -> { x with phone }
      | Phone2 phone2 -> { x with phone2 }
      | Mobile mobile -> { x with mobile }
      | Fax fax -> { x with fax }
      | Fax2 fax2 -> { x with fax2 }
      | Mail mail -> { x with mail }
      | Mail2 mail2 -> { x with mail2 }
      | Web web -> { x with web }
    in
    { x with contact }
  | Company action ->
    let company =
      let x = x.company in
      match action with
      | Company_name name -> { x with name }
      | Company_address address -> { x with address }
    in
    { x with company }
  | Note note -> { x with note }
  | Keyword keyword -> { x with keyword }
;;

let apply_action
    ~bookings
    (model : Model.t)
    (action : Action.t)
    (state : State.t)
    ~schedule_action
    : Model.t
  =
  let conn = state.connection in
  match action with
  | Customer action -> { model with local = apply_form_action model.local action }
  | Touched ->
    (* We waited some time after a touch *)
    let () =
      if Browser.Date.(to_int (now ())) - model.touched_at >= 300
      then schedule_action Action.Save
      else ()
    in
    model
  | Touch ->
    (* Form touched. Wait a bit. Avoid high frequent saving. *)
    let () =
      Async_kernel.(
        don't_wait_for
          (after (Time_ns.Span.of_sec 0.3) >>| fun () -> schedule_action Action.Touched))
    in
    { model with sync = false; touched_at = Browser.Date.(to_int (now ())) }
  | Save -> model
  (* TODO: saving
    let c_opt, form = Form.State.read_value model.form customer_form in
    let model = { model with form } in
    Log.form form;
    (match c_opt with
    | None -> model
    | Some local ->
      let () =
        match model.nav with
        | New, _ ->
          let handler = Fn.compose schedule_action Action.postedcustomer in
          Request.XHR.send ~body:local ~handler Remote.(Customer.post |> finalize conn)
        | Id id, _ ->
          let handler = Fn.compose schedule_action Action.patchedcustomer in
          Request.XHR.send
            ~body:local
            ~handler
            Remote.(Customer.patch id |> finalize conn)
      in
      { model with local }) *)
  | NewBooking ->
    let init =
      let i =
        match snd model.nav with
        | Booking (i, _) -> i
        | _ -> 0
      in
      match List.nth bookings i with
      | Some c ->
        let b : Booking.t = Component.extra c in
        { b with
          period = default_period ()
        ; deposit_asked = None
        ; deposit_got = None
        ; note = ""
        }
      | None -> fresh_booking ()
    in
    let bookings = (Model.Fresh, Booking_form.Model.load init) :: model.bookings
    and nav = fst model.nav, Nav.(Booking (0, BData)) in
    schedule_action Action.Touch;
    { model with bookings; nav }
  | DeleteBooking i ->
    let bookings = List.filteri ~f:(fun j _ -> i <> j) model.bookings in
    let nav = fst model.nav, Nav.(Booking (min i (List.length bookings - 1), BData)) in
    schedule_action Action.Touch;
    { model with bookings; nav }
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
        Request.XHR.send' ~handler Remote.(Customer.delete i |> finalize conn)
    in
    schedule_action Action.Touch;
    model
  | PatchedCustomer (Ok remote) -> { model with remote; sync = model.local = remote }
  | PostedCustomer (Ok (id, remote)) ->
    { model with remote; nav = Nav.(Id id, snd model.nav); sync = model.local = remote }
  | GotCustomer (Ok (id, remote)) -> Model.load Nav.(Id id, snd model.nav) remote
  | PostedCustomer (Error detail) | PatchedCustomer (Error detail) ->
    state.handle_error { gist = "Speichern fehlgeschlagen"; detail };
    model
  | GotCustomer (Error detail) ->
    state.handle_error { gist = "Laden fehlgeschlagen"; detail };
    model
  | NavChange ((New, _) as nav) -> Model.create' ~nav ()
  | NavChange ((Id i, _) as nav) when Nav.Id i <> fst model.nav ->
    let rq =
      Request.map_resp ~f:(fun c -> i, c) Remote.(Customer.get i |> finalize conn)
    in
    let handler = Fn.compose schedule_action Action.gotcustomer in
    Request.XHR.send' ~handler rq;
    Model.create' ~loading:true ~nav ()
  | NavChange nav -> { model with nav }
  | Booking (i, a) ->
    let schedule_action = Fn.compose schedule_action (Action.booking i) in
    let bookings =
      List.mapi model.bookings ~f:(fun j (v, m) ->
          if i <> j
          then v, m
          else (
            match List.nth bookings i with
            | Some c -> v, Component.apply_action ~schedule_action c a state
            | None -> v, m))
    in
    { model with bookings }
;;

module Fields = struct
  open Interactive
  open Vdom

  (* This is called once on Incr node construction, I hope. Use this fact for
   * deriving id's and link labels *)

  let input ?init label =
    let attrs = [ Attr.class_ "form-control" ] in
    Primitives.text ?init ~attrs ()
    |> map_nodes ~f:(fun nodes -> Node.label [] [ Node.text label ] :: nodes)
    |> wrap_in_div ~attrs:[ Attr.class_ "form-group" ]
  ;;

  let textarea ?init ~nrows label =
    let attrs =
      let open Attr in
      [ class_ "form-control"; create "rows" (string_of_int nrows) ]
    in
    Primitives.text_area ?init ~attrs ()
    |> map_nodes ~f:(fun nodes -> Node.label [] [ Node.text label ] :: nodes)
    |> wrap_in_div ~attrs:[ Attr.class_ "form-group" ]
  ;;
end

open Vdom

module View = struct
  open Action
  open Fields
  open Interactive
  open Incr.Let_syntax

  let name ~inject (x : Customer.Name.t) =
    let inject a = inject (Name a) in
    let%map title = render (input ~init:x.title "Titel") ~inject ~on_input:title
    and letter = render (input "Anrede Brief") ~inject ~on_input:letter
    and given = render (input "Vorname") ~inject ~on_input:given
    and family = render (input "Nachname") ~inject ~on_input:family in
    Bs.Grid.
      [ frow [ col4 [ title ]; col8 [ letter ] ]; frow [ col [ given ]; col [ family ] ] ]
  ;;

  let company ~inject =
    let inject a = inject (Company a) in
    let%map name = render (input "Firma") ~inject ~on_input:company_name
    and address = render (input "Abteilung") ~inject ~on_input:company_address in
    Bs.Grid.[ frow [ col [ name ] ]; frow [ col [ address ] ] ]
  ;;

  let address ~inject =
    let inject a = inject (Address a) in
    let%map street_with_num =
      render (input "Straße und Hausnummer") ~inject ~on_input:street_with_num
    and postal_code = render (input "Postleitzahl") ~inject ~on_input:postal_code
    and city = render (input "Ort") ~inject ~on_input:city
    and country = render (input "Land") ~inject ~on_input:country
    and country_code = render (input "Code") ~inject ~on_input:country_code in
    Bs.Grid.
      [ frow [ col [ street_with_num ] ]
      ; frow [ col4 [ postal_code ]; col8 [ city ] ]
      ; frow [ col8 [ country ]; col4 [ country_code ] ]
      ]
  ;;

  let contact ~inject =
    let inject a = inject (Contact a) in
    let%map phone = render (input "Telefon") ~inject ~on_input:phone
    and phone2 = render (input "Telefon") ~inject ~on_input:phone2
    and mobile = render (input "Mobile") ~inject ~on_input:mobile
    and fax = render (input "Fax") ~inject ~on_input:fax
    and fax2 = render (input "Fax") ~inject ~on_input:fax2
    and mail = render (input "Mail") ~inject ~on_input:mail
    and mail2 = render (input "Mail") ~inject ~on_input:mail2
    and web = render (input "Internet") ~inject ~on_input:web in
    Bs.Grid.
      [ frow [ col [ phone ]; col [ phone2 ] ]
      ; frow [ col [ mobile ] ]
      ; frow [ col [ fax ]; col [ fax2 ] ]
      ; frow [ col [ mail ] ]
      ; frow [ col [ mail2 ] ]
      ; frow [ col [ web ] ]
      ]
  ;;

  let customer ~inject (x : Customer.t) =
    let inject a = inject (Customer a) in
    let%map name = name ~inject x.name
    and company = company ~inject
    and address = address ~inject
    and contact = contact ~inject
    and keyword = render (input "Schlüsselwort") ~inject ~on_input:keyword
    and note = render (textarea ~nrows:8 "Notiz") ~inject ~on_input:note in
    let left = Bs.Grid.((frow [ col [ keyword ] ] :: name) @ [ frow [ col [ note ] ] ])
    and middle = address @ company
    and right = contact in
    Bs.Grid.[ row [ col left; col middle; col right ] ]
  ;;
end

let danger_btn action title =
  Bs.button ~attr:[ Bs.tab_skip ] ~style:"outline-danger" ~action title
;;

let save_btn ~sync ~inject =
  let action _ = inject Action.Save in
  if sync
  then Bs.button ~action ~i:(S "check") ~style:"outline-success" "Gespeichert"
  else Bs.button ~action ~i:(S "save") ~style:"outline-warning" "Speichern"
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

let view ~sync ~inject customer =
  let delete_c _evt = inject Action.DeleteCustomer
  and new_b _evt = inject Action.NewBooking in
  let%map form = View.customer ~inject Customer.empty
  (* TODO: changing the init customer triggers rebuild of all form fields.
     Probably we need a model.init that is only changed on load. *)
  and letter_dropdown = customer >>| letter_dropdown
  and sync = sync in
  Bs.Grid.(
    (Node.h4 [] [ Node.text "Stammdaten" ] :: Node.hr [] :: form)
    @ [ frow
          [ col_auto ~c:[ "mb-2"; "mt-2" ] [ letter_dropdown ]
          ; col
              [ frow
                  ~c:[ "justify-content-end" ]
                  [ col_auto
                      ~c:[ "mb-2"; "mt-2" ]
                      [ danger_btn delete_c "Kunde löschen" ]
                  ; col_auto
                      ~c:[ "mb-2"; "mt-2" ]
                      [ Bs.button ~action:new_b "Neue Buchung" ]
                  ; col_auto ~c:[ "mb-2"; "mt-2" ] [ save_btn ~sync ~inject ]
                  ]
              ]
          ]
      ])
;;

let view_form ~bookings (model : Model.t Incr.t) ~inject =
  let open Vdom in
  let%map form =
    let sync = model >>| Model.sync in
    view ~sync ~inject (model >>| Model.local)
  and bookings = bookings
  and nav = model >>| Model.nav >>| snd in
  let save _evt = inject Action.Save in
  let rows =
    match nav with
    | CData -> form
    | Booking (i, _) ->
      (match List.nth bookings i with
      | Some b -> [ Component.view b ]
      | None -> [])
  in
  let touch _ _ = inject Action.Touch in
  Node.create "form" [ Attr.on "submit" save; Attr.on_input touch ] rows
;;

let view ~bookings ~inject model =
  let%bind loading = model >>| Model.loading in
  if loading then Incr.const Bs.Grid.loading_row else view_form ~bookings ~inject model
;;

let menu (m : Model.t) : Menu.t =
  let open Menu in
  let href nav = Href (Nav.href (Customer (fst m.nav, nav))) in
  let goto_main = href CData in
  let children =
    let f i v =
      let title =
        match fst v with
        | Model.Fresh -> "Neue Buchung"
        | Saved p -> Period.to_string_hum ~sep:"-" p
      and action = href (Booking (i, BData)) in
      entry
        title
        action
        (match snd m.nav with
        | Booking (j, _) when j = i -> true
        | _ -> false)
    in
    entry "Stammdaten" goto_main (snd m.nav = CData) :: List.mapi ~f m.bookings
  in
  let title =
    if m.loading
    then "Kunde lädt ..."
    else (
      match String.strip m.local.keyword with
      | "" -> "Kunde: n/a"
      | s -> "Kunde: " ^ s)
  in
  [ entry ~children title goto_main false ]
;;

let create ~(inject : Action.t -> Vdom.Event.t)
           (model : Model.t Incr.t) =
  let bookings =
    (* This cannot be efficient. Fix? TODO *)
    let%bind bookings = model >>| Model.bookings
    and nav =
      model
      >>| Model.nav
      >>| function
      | _, Booking (_, bnav) -> bnav
      | _ -> Nav.BData
    in
    List.mapi bookings ~f:(fun i (_, b) ->
        let inject = Fn.compose inject (Action.booking i)
        and env : Booking_form.env =
          { nav = Incr.const nav
          ; customer = model >>| Model.local
          ; new_booking =
              Fn.compose inject (fun _b -> Action.newbooking)
              (* TODO: use _b booking proposed by child component *)
          ; delete_booking = Fn.compose inject (fun () -> Action.deletebooking i)
          }
        in
        Booking_form.create ~env ~inject (Incr.const b))
    |> Incr.all
  in
  let%map model = model
  and bookings = bookings
  and view = view ~bookings ~inject model in
  let apply_action = apply_action ~bookings model
  and extra : Menu.t * Customer.t =
    let c = model.local in
    let bookings = List.map ~f:Component.extra bookings in
    menu model, { c with bookings }
  in
  Component.create_with_extra ~apply_action ~extra model view
;;
