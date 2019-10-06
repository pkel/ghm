(* TODO:
   - database interaction should NOT be handled here
   - nav should not be part of the model
   - nav should be an incremental argument to the component
   - local customer is exposed via extra, upper level should observe
     changes and handle database interaction
   - customer form is broken (switch to booking then back to CData)
   - saving of booking is broken
   - (done) there should be one invoice form per booking
   - (done) the exposed extra customer should integrate the exposed incremental
     invoices/bookings of the sub component
*)

open Core_kernel
open Ghm
open Incr_dom
open Incr_dom_widgets
open Incr.Let_syntax

let last_id = ref (-1)

let id () =
  incr last_id;
  sprintf "customer_form_%i" !last_id
;;

module Form_description = struct
  let name =
    let open Form.Description in
    let unvalidated =
      let open Of_record in
      build_for_record
        (Customer.Name.Fields.make_creator
           ~title:(field string)
           ~given:(field string)
           ~letter:(field string)
           ~family:(field string))
    in
    conv unvalidated ~f:(fun t _ ~block_id:_ ->
        let errors = [] in
        if List.is_empty errors then Ok t else Error errors)
  ;;

  let company =
    let open Form.Description in
    let unvalidated =
      let open Of_record in
      build_for_record
        (Customer.Company.Fields.make_creator
           ~name:(field string)
           ~address:(field string))
    in
    conv unvalidated ~f:(fun t _ ~block_id:_ ->
        let errors = [] in
        if List.is_empty errors then Ok t else Error errors)
  ;;

  let address =
    let open Form.Description in
    let unvalidated =
      let open Of_record in
      build_for_record
        (Customer.Address.Fields.make_creator
           ~street_with_num:(field string)
           ~postal_code:(field string)
           ~city:(field string)
           ~country:(field string)
           ~country_code:(field string))
    in
    conv unvalidated ~f:(fun t _ ~block_id:_ ->
        let errors = [] in
        if List.is_empty errors then Ok t else Error errors)
  ;;

  let contact =
    let open Form.Description in
    let unvalidated =
      let open Of_record in
      build_for_record
        (Customer.Contact.Fields.make_creator
           ~phone:(field string)
           ~phone2:(field string)
           ~mobile:(field string)
           ~fax:(field string)
           ~fax2:(field string)
           ~mail:(field string)
           ~mail2:(field string)
           ~web:(field string))
    in
    conv unvalidated ~f:(fun t _ ~block_id:_ ->
        let errors = [] in
        if List.is_empty errors then Ok t else Error errors)
  ;;

  let nonempty_string errmsg =
    let open Form.Description in
    conv_without_block string ~f:(fun s id ->
        match s with
        | "" -> Error [ Form.Form_error.create ~id (Error.of_string errmsg) ]
        | s -> Ok s)
  ;;

  let customer =
    let open Form.Description in
    let unvalidated =
      let open Of_record in
      build_for_record
        (Customer.Fields.make_creator
           ~name:(field name)
           ~address:(field address)
           ~company:(field company)
           ~contact:(field contact)
           ~keyword:(field (nonempty_string "Schlüsselwort darf nicht leer sein"))
           ~note:(field string)
           ~bookings:(field (not_editable ~default:[])))
    in
    conv unvalidated ~f:(fun t _ ~block_id:_ -> Ok t)
  ;;
end

let customer_form = Form.create ~name:"customer form" Form_description.customer

module Model = struct
  type visit =
    | Fresh
    | Saved of Period.t
  [@@deriving compare]

  type t =
    { form : Form.State.t
    ; remote : Customer.t
    ; local : Customer.t
    ; bookings : (visit * Booking_form.Model.t) list
    ; nav : Nav.noi * Nav.customer
    ; touched_at : int
    ; sync : bool
    ; loading : bool
    }
  [@@deriving compare, fields]

  let load nav (c : Customer.t) =
    { form = Form.State.create ~init:c customer_form
    ; remote = c
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
  type t =
    | FormUpdate of Form.State.t sexp_opaque
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
  | Save ->
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
      { model with local })
  | FormUpdate form ->
    schedule_action Action.Touch;
    { model with form }
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

open Vdom

let err_of_block state id =
  let msg err =
    String.strip ~drop:(fun c -> Char.equal c '"') (Error.to_string_hum err)
  in
  Option.map (Form.State.error state id) ~f:(fun x -> Node.text (msg x))
;;

let err_div_of_block state id =
  Option.map (err_of_block state id) ~f:(fun x ->
      Node.div [ Attr.classes [ "alert"; "alert-danger" ] ] [ x ])
;;

let prepend_err_div state (id : Form.Block.t Form.Id.t) rows =
  match err_div_of_block state id with
  | Some x -> x :: rows
  | None -> rows
;;

let group = Node.div [ Attr.class_ "form-group" ]

let input_str
    ?(attr = []) ?(input = Form.Input.text) ?(type_ = "text") ?datalist state label id
  =
  let classes, error =
    match err_of_block state id with
    | None -> [], []
    | Some m -> [ "is-invalid" ], [ Node.div [ Attr.class_ "invalid-feedback" ] [ m ] ]
  in
  let attr, datalist =
    match datalist with
    | None -> attr, []
    | Some (id, l) ->
      ( Attr.create "list" id :: attr
      , [ Node.create
            "datalist"
            [ Attr.id id ]
            (List.map l ~f:(fun s -> Node.create "option" [ Attr.value s ] []))
        ] )
  in
  group
    ([ Form.Input.label id [] [ Node.text label ]
     ; input
         state
         id
         (Attr.classes ("form-control" :: classes) :: Attr.type_ type_ :: attr)
     ]
    @ error
    @ datalist)
;;

let dl1_id = id ()
let dl2_id = id ()

let view_name state ids =
  let block, (title, (letter, (given, (family, ())))) = ids in
  let titles, letters = List.unzip Customer.letter_by_title in
  let titles = dl1_id, titles in
  let letters = dl2_id, letters in
  Bs.Grid.
    [ frow [ col (prepend_err_div state block []) ]
    ; frow
        [ col4 [ input_str ~datalist:titles state "Titel" title ]
        ; col8 [ input_str ~datalist:letters state "Anrede Brief" letter ]
        ]
    ; frow
        [ col [ input_str state "Vorname" given ]
        ; col [ input_str state "Nachname" family ]
        ]
    ]
;;

let view_company state ids =
  let block, (name, (address, ())) = ids in
  Bs.Grid.
    [ frow [ col (prepend_err_div state block []) ]
    ; frow [ col [ input_str state "Firma" name ] ]
    ; frow [ col [ input_str state "Abteilung" address ] ]
    ]
;;

let view_address state ids =
  let block, (street_with_num, (postal_code, (city, (country, (country_code, ()))))) =
    ids
  in
  Bs.Grid.
    [ frow [ col (prepend_err_div state block []) ]
    ; frow [ col [ input_str state "Straße und Hausnummer" street_with_num ] ]
    ; frow
        [ col4 [ input_str state "Postleitzahl" postal_code ]
        ; col8 [ input_str state "Ort" city ]
        ]
    ; frow
        [ col8 [ input_str state "Land" country ]
        ; col4 [ input_str state "Code" country_code ]
        ]
    ]
;;

let view_contact state ids =
  let block, (phone, (phone2, (mobile, (fax, (fax2, (mail, (mail2, (web, ())))))))) =
    ids
  in
  Bs.Grid.
    [ frow [ col (prepend_err_div state block []) ]
    ; frow
        [ col [ input_str state "Telefon" phone ]
        ; col [ input_str state "Telefon" phone2 ]
        ]
    ; frow [ col [ input_str state "Mobil" mobile ] ]
    ; frow [ col [ input_str state "Fax" fax ]; col [ input_str state "Fax" fax2 ] ]
    ; frow [ col [ input_str state "Mail" mail ] ]
    ; frow [ col [ input_str state "Mail" mail2 ] ]
    ; frow [ col [ input_str state "Internet" web ] ]
    ]
;;

let textarea n state id attr =
  Form.Input.textarea state id (Attr.create "rows" (string_of_int n) :: attr)
;;

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

let letter_dropdown_id = id ()

let view_main ~sync ~inject customer state ids =
  let block, (name, (company, (address, (contact, (keyword, (note, (_, ()))))))) = ids
  and delete_c _evt = inject Action.DeleteCustomer
  and new_b _evt = inject Action.NewBooking in
  let left =
    Bs.Grid.(
      (frow [ col [ input_str state "Schlüsselwort" keyword ] ] :: view_name state name)
      @ [ frow [ col [ input_str ~input:(textarea 8) state "Notiz" note ] ] ])
  and middle = view_address state address @ view_company state company
  and right = view_contact state contact
  and letter_dropdown =
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
  in
  Bs.Grid.
    [ Node.h4 [] [ Node.text "Stammdaten" ]
    ; Node.hr []
    ; row (prepend_err_div state block [])
    ; row [ col left; col middle; col right ]
    ; frow
        [ col_auto ~c:[ "mb-2"; "mt-2" ] [ letter_dropdown ]
        ; col
            [ frow
                ~c:[ "justify-content-end" ]
                [ col_auto ~c:[ "mb-2"; "mt-2" ] [ danger_btn delete_c "Kunde löschen" ]
                ; col_auto
                    ~c:[ "mb-2"; "mt-2" ]
                    [ Bs.button ~action:new_b "Neue Buchung" ]
                ; col_auto ~c:[ "mb-2"; "mt-2" ] [ save_btn ~sync ~inject ]
                ]
            ]
        ]
    ]
;;

let view_form ~bookings (model : Model.t Incr.t) ~inject =
  let open Vdom in
  let%map form = model >>| Model.form
  and local = model >>| Model.local
  and sync = model >>| Model.sync
  and bookings = bookings
  and nav = model >>| Model.nav >>| snd in
  let ids = Form.State.field_ids form customer_form in
  let save _evt = inject Action.Save in
  let rows =
    match nav with
    | CData -> view_main ~inject ~sync local form ids
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
