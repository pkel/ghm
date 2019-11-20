(* TODO:
   - nav should not be part of the model
   - nav should be an incremental argument to the component

   - Use Interactive monad to filter empty keywords and display errors to user
*)

open Core_kernel
open Ghm
open Incr_dom
open Incr_dom_widgets
open Incr.Let_syntax

module Model = struct
  type t =
    { remote : Pg.Customers.return
    ; init : int (* times loaded, triggers recreation of form elements *) * Customer.t
    ; local : Customer.t
    ; last_input_at : int
    ; nav : Nav.noi * Nav.customer
    ; is_loading : bool (* ; booking : Booking_form.Model.t *)
    }
  [@@deriving compare, fields]

  let init_id =
    let i = ref 0 in
    function
    | () ->
      incr i;
      !i
  ;;

  let t
      ?(nav = Nav.(New, CData))
      ?(is_loading = false)
      ?(c = { Pg.Customers.id = -1; data = Customer.empty; bookings = [] })
      ()
    =
    { remote = c
    ; local = c.data
    ; init = init_id (), c.data
    ; last_input_at = Int.max_value
    ; nav
    ; is_loading
    }
  ;;

  let loading nav = t ~is_loading:true ~nav ()
  let loaded m c = t ~nav:m.nav ~is_loading:false ~c ()
  let create () = t ()
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
    | Delayed_after_input
    | DeleteCustomer
    | Save
    | PostedCustomer of Pg.Customers.return sexp_opaque Or_error.t
    | PatchedCustomer of Pg.Customers.return sexp_opaque Or_error.t
    | NavChange of (Nav.noi * Nav.customer) sexp_opaque
    | GotCustomer of Pg.Customers.return sexp_opaque Or_error.t
  [@@deriving sexp_of, variants]
end

let apply_customer_action x =
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
  | Keyword keyword ->
    (match String.strip keyword with
    | "" -> x (* TODO: Signal to user *)
    | keyword -> { x with keyword })
;;

let apply_action (model : Model.t) (action : Action.t) (state : State.t) ~schedule_action
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
  | Customer action ->
    delay_after_input { model with local = apply_customer_action model.local action }
  | Delayed_after_input ->
    let () =
      if Browser.Date.(to_int (now ())) - model.last_input_at >= 300
      then schedule_action Action.Save
      else ()
    in
    model
  | Save ->
    let () =
      if model.local <> model.remote.data
      then (
        let c = state.connection in
        if model.remote.id < 0
        then (
          let handler = Fn.compose schedule_action Action.postedcustomer in
          Xhr.send ~c ~body:model.local ~handler Pg.(create Customers.t))
        else (
          let handler = Fn.compose schedule_action Action.patchedcustomer in
          Xhr.send
            ~c
            ~body:model.local
            ~handler
            Pg.(update' Int.(Customers.id' == model.remote.id) Customers.t)))
    in
    model
  | PatchedCustomer (Ok remote) -> { model with remote }
  | PostedCustomer (Ok remote) ->
    let nav = Nav.Id remote.id, snd model.nav in
    let () = Nav.set (Customer nav) in
    { model with remote; nav }
  | PostedCustomer (Error detail) | PatchedCustomer (Error detail) ->
    state.handle_error { gist = "Speichern fehlgeschlagen"; detail };
    model
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
  | NavChange (New, _) -> Model.create ()
  | NavChange ((Id i, _) as nav) when Nav.Id i <> fst model.nav ->
    let rq = Pg.(read' Int.(Customers.id' == i) Customers.t) in
    let handler = Fn.compose schedule_action Action.gotcustomer in
    let c = state.connection in
    Xhr.send' ~c ~handler rq;
    Model.loading nav
  | NavChange nav -> { model with nav }
  | GotCustomer (Ok return) -> Model.loaded model return
  | GotCustomer (Error detail) ->
    state.handle_error { gist = "Laden fehlgeschlagen"; detail };
    model
;;

open Vdom

module View = struct
  open Action
  open Bs.Form
  open Interactive
  open Incr.Let_syntax

  let name ~inject ~(init : Customer.Name.t) =
    let x = init in
    let inject a = inject (Name a) in
    let%map title = render (input ~init:x.title "Titel") ~inject ~on_input:title
    and letter = render (input ~init:x.letter "Anrede Brief") ~inject ~on_input:letter
    and given = render (input ~init:x.given "Vorname") ~inject ~on_input:given
    and family = render (input ~init:x.family "Nachname") ~inject ~on_input:family in
    Bs.Grid.
      [ frow [ col4 [ title ]; col8 [ letter ] ]; frow [ col [ given ]; col [ family ] ] ]
  ;;

  let company ~inject ~(init : Customer.Company.t) =
    let x = init in
    let inject a = inject (Company a) in
    let%map name = render (input ~init:x.name "Firma") ~inject ~on_input:company_name
    and address =
      render (input ~init:x.address "Abteilung") ~inject ~on_input:company_address
    in
    Bs.Grid.[ frow [ col [ name ] ]; frow [ col [ address ] ] ]
  ;;

  let address ~inject ~(init : Customer.Address.t) =
    let x = init in
    let inject a = inject (Address a) in
    let%map street_with_num =
      render
        (input ~init:x.street_with_num "Straße und Hausnummer")
        ~inject
        ~on_input:street_with_num
    and postal_code =
      render (input ~init:x.postal_code "Postleitzahl") ~inject ~on_input:postal_code
    and city = render (input ~init:x.city "Ort") ~inject ~on_input:city
    and country = render (input ~init:x.country "Land") ~inject ~on_input:country
    and country_code =
      render (input ~init:x.country_code "Code") ~inject ~on_input:country_code
    in
    Bs.Grid.
      [ frow [ col [ street_with_num ] ]
      ; frow [ col4 [ postal_code ]; col8 [ city ] ]
      ; frow [ col8 [ country ]; col4 [ country_code ] ]
      ]
  ;;

  let contact ~inject ~(init : Customer.Contact.t) =
    let x = init in
    let inject a = inject (Contact a) in
    let%map phone = render (input ~init:x.phone "Telefon") ~inject ~on_input:phone
    and phone2 = render (input ~init:x.phone2 "Telefon") ~inject ~on_input:phone2
    and mobile = render (input ~init:x.mobile "Mobile") ~inject ~on_input:mobile
    and fax = render (input ~init:x.fax "Fax") ~inject ~on_input:fax
    and fax2 = render (input ~init:x.fax2 "Fax") ~inject ~on_input:fax2
    and mail = render (input ~init:x.mail "Mail") ~inject ~on_input:mail
    and mail2 = render (input ~init:x.mail2 "Mail") ~inject ~on_input:mail2
    and web = render (input ~init:x.web "Internet") ~inject ~on_input:web in
    Bs.Grid.
      [ frow [ col [ phone ]; col [ phone2 ] ]
      ; frow [ col [ mobile ] ]
      ; frow [ col [ fax ]; col [ fax2 ] ]
      ; frow [ col [ mail ] ]
      ; frow [ col [ mail2 ] ]
      ; frow [ col [ web ] ]
      ]
  ;;

  let customer ~inject ~(init : Customer.t) =
    let x = init in
    let inject a = inject (Customer a) in
    let%map name = name ~inject ~init:x.name
    and company = company ~inject ~init:x.company
    and address = address ~inject ~init:x.address
    and contact = contact ~inject ~init:x.contact
    and keyword =
      let validator value =
        match String.strip value with
        | "" -> Some "Das Schlüsselwort darf nicht leer sein!"
        | _ -> None
      in
      render (input ~validator ~init:x.keyword "Schlüsselwort") ~inject ~on_input:keyword
    and note = render (textarea ~init:x.note ~nrows:8 "Notiz") ~inject ~on_input:note in
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
  let action _ = inject Action.(Save) in
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

let view ~sync ~inject ~init customer =
  let delete_c _evt = inject Action.DeleteCustomer in
  let%map form =
    let%bind _, init = init in
    View.customer ~inject ~init
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
                  ; col_auto ~c:[ "mb-2"; "mt-2" ] [ save_btn ~sync ~inject ]
                  ]
              ]
          ]
      ])
;;

let view_form (model : Model.t Incr.t) ~inject =
  let open Vdom in
  let%map form =
    let sync =
      let%map a = model >>| fun Model.{ remote = { data; _ }; _ } -> data
      and b = model >>| Model.local in
      a = b
    and init = model >>| Model.init in
    view ~sync ~inject ~init (model >>| Model.local)
  in
  let save _evt = inject Action.(Save) in
  Node.create "form" [ Attr.on "submit" save ] form
;;

let view ~inject model =
  let cond = model >>| Model.is_loading in
  Incr.if_ cond ~then_:(Incr.const Bs.Grid.loading_row) ~else_:(view_form ~inject model)
;;

let menu (m : Model.t) : Menu.t =
  let open Menu in
  let open Nav in
  let href nav = Href (Nav.href (Customer (fst m.nav, nav))) in
  let children =
    entry "Stammdaten" (href CData) (snd m.nav = CData)
    :: entry "Neue Buchung" (href (Booking (New, BData))) false
    :: List.map m.remote.bookings ~f:(fun { arrival; departure; id } ->
           entry
             Period.(to_string_hum (of_dates arrival departure))
             (href (Booking (Id id, BData)))
             false)
  in
  let title =
    if m.is_loading
    then "Kunde lädt ..."
    else (
      match String.strip m.local.keyword with
      | "" -> "Kunde: n/a"
      | s -> "Kunde: " ^ s)
  in
  [ entry ~children title (href CData) false ]
;;

let create ~(inject : Action.t -> Vdom.Event.t) (model : Model.t Incr.t) =
  let%map model = model
  and view = view ~inject model in
  let apply_action = apply_action model
  and extra : Menu.t = menu model in
  Component.create_with_extra ~apply_action ~extra model view
;;
