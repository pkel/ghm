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

  let opt_date =
    let open Form.Description in
    conv_without_block string ~f:(fun s id ->
        if s = ""
        then Ok None
        else (
          match Date.of_string s with
          | d -> Ok (Some d)
          | exception _ ->
            Error
              [ Form.Form_error.create ~id (Error.of_string "Datum nicht auswertbar.") ])
    )
    |> contra_map ~f:(function
           | None -> ""
           | Some d -> Date.to_string d)
  ;;

  let period =
    let open Form.Description in
    let unvalidated =
      let open Let_syntax in
      let%map_open from = opt_date <^ Fn.compose Option.some Period.from
      and till = opt_date <^ Fn.compose Option.some Period.till in
      from, till
    in
    conv_without_block unvalidated ~f:(fun (f, t) _ ->
        let default = Date.(add_days (today ~zone:Time.Zone.utc) 7) in
        let f, t =
          match f, t with
          | None, None -> default, Date.add_days default 1
          | None, Some d | Some d, None -> d, Date.add_days d 1
          | Some a, Some b -> a, b
        in
        Ok (Period.of_dates f t))
  ;;

  let monetary_opt =
    let open Form.Description in
    conv_without_block string ~f:(fun s id ->
        let e =
          Error [ Form.Form_error.create ~id (Error.of_string "Geldwert erwartet.") ]
        in
        if s = ""
        then Ok None
        else (
          match float_of_string s with
          | x ->
            (match Monetary.of_float x with
            | Some m -> Ok (Some m)
            | None -> e)
          | exception _ -> e))
    |> contra_map ~f:(function
           | None -> ""
           | Some x -> Monetary.to_string_dot x)
  ;;

  let monetary =
    let open Form.Description in
    conv_without_block monetary_opt ~f:(fun opt _id ->
        match opt with
        | None -> Ok Monetary.zero
        | Some m -> Ok m)
    |> contra_map ~f:(fun x -> if Monetary.(compare zero x) = 0 then None else Some x)
  ;;

  let int =
    let open Form.Description in
    conv_without_block string ~f:(fun s id ->
        if s = ""
        then Ok 0
        else (
          match int_of_string s with
          | i -> Ok i
          | exception _ ->
            Error [ Form.Form_error.create ~id (Error.of_string "Ganze Zahl erwartet.") ])
    )
    |> contra_map ~f:(function
           | 0 -> ""
           | i -> string_of_int i)
  ;;

  let guest =
    let open Form.Description in
    let unvalidated =
      Of_record.(
        build_for_record
          (Booking.Fields_of_guest.make_creator
             ~given:(field string)
             ~family:(field string)
             ~born:(field opt_date)))
    in
    conv unvalidated ~f:(fun t _ ~block_id:_ -> Ok t)
  ;;

  let alloc =
    let open Form.Description in
    let unvalidated =
      Of_record.(
        build_for_record
          (Booking.Fields_of_alloc.make_creator
             ~room:(field string)
             ~beds:(field int)
             ~price_per_bed:(field monetary)
             ~description:(field string)))
    in
    conv unvalidated ~f:(fun t _ ~block_id:_ -> Ok t)
  ;;

  let booking =
    let open Form.Description in
    let unvalidated =
      Of_record.(
        build_for_record
          (Booking.Fields.make_creator
             ~deposit_asked:(field monetary_opt)
             ~deposit_got:(field monetary_opt)
             ~tax_free:(field bool)
             ~note:(field string)
             ~guests:(field (list guest))
             ~allocs:(field (list alloc))
             ~period:(field period)))
    in
    conv unvalidated ~f:(fun b _ ~block_id:_ -> Ok b)
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
           ~bookings:(field (list booking)))
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

  type view =
    | Main
    | Invoice of int
    | Booking of int
    | New_booking
  [@@deriving compare, sexp]

  type t =
    { form : Form.State.t
    ; invoice_form : Invoice_form.Model.t
    ; remote : Customer.t
    ; local : Customer.t
    ; view : view
    ; bookings : visit list
    ; nav : Nav.customer
    ; touched_at : int
    ; sync : bool
    ; loading : bool
    }
  [@@deriving compare, fields]

  let load nav (c : Customer.t) =
    { form = Form.State.create ~init:c customer_form
    ; invoice_form = Invoice_form.Model.create ()
    ; remote = c
    ; local = c
    ; nav
    ; touched_at = Int.max_value
    ; view = Main
    ; bookings = List.map ~f:(fun b -> Saved (Booking.period b)) c.bookings
    ; loading = false
    ; sync = true
    }
  ;;

  let create () = load New Customer.empty

  let create_loading nav () =
    let model = load nav Customer.empty in
    { model with loading = true }
  ;;
end

module Action = struct
  type t =
    | FormUpdate of Form.State.t sexp_opaque
    | ChangeView of Model.view
    | NavChange of Nav.customer sexp_opaque
    | GotCustomer of (int * Customer.t) Or_error.t
    | PostedCustomer of (int * Customer.t) Or_error.t
    | PatchedCustomer of Customer.t Or_error.t
    | InvoiceForm of Invoice_form.Action.t sexp_opaque
    | Save
    | Touch
    | Touched
    | NewBooking
    | DeleteBooking of int
    | DeleteCustomer
    | LoadInvoice of int
  [@@deriving sexp, variants]
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
    }
;;

let booking_list_id state =
  let _, (_, (_, (_, (_, (_, (_, ((_, r), ()))))))) =
    Form.State.field_ids state customer_form
  in
  r
;;

let booking_entry_ids state =
  let _, (_, (_, (_, (_, (_, (_, ((r, _), ()))))))) =
    Form.State.field_ids state customer_form
  in
  r
;;

let apply_action
    ~invoice
    (model : Model.t)
    (action : Action.t)
    (state : State.t)
    ~schedule_action
    : Model.t
  =
  let conn = state.connection in
  match action with
  | ChangeView view -> { model with view }
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
        | New ->
          let handler = Fn.compose schedule_action Action.postedcustomer in
          Request.XHR.send ~body:local ~handler Remote.(Customer.post |> finalize conn)
        | Id id ->
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
        match model.view with
        | Booking i -> i
        | _ -> 0
      in
      match List.nth model.local.bookings i with
      | Some b ->
        { b with
          period = default_period ()
        ; deposit_asked = None
        ; deposit_got = None
        ; note = ""
        }
      | None -> fresh_booking ()
    in
    let form = Form.List.cons ~init model.form (booking_list_id model.form)
    and bookings = Model.Fresh :: model.bookings
    and view = Model.Booking 0 in
    schedule_action Action.Touch;
    { model with form; bookings; view }
  | DeleteBooking i ->
    let form = Form.List.remove_nth model.form (booking_list_id model.form) i
    and bookings = List.filteri ~f:(fun j _ -> i <> j) model.bookings in
    let view = Model.Booking (min i (List.length bookings - 1)) in
    schedule_action Action.Touch;
    { model with form; bookings; view }
  | DeleteCustomer ->
    let () =
      match model.nav with
      | New -> Nav.(set Overview)
      | Id i ->
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
    { model with remote; nav = Nav.Id id; sync = model.local = remote }
  | GotCustomer (Ok (id, remote)) -> Model.load (Id id) remote
  | PostedCustomer (Error detail) | PatchedCustomer (Error detail) ->
    state.handle_error { gist = "Speichern fehlgeschlagen"; detail };
    model
  | GotCustomer (Error detail) ->
    state.handle_error { gist = "Laden fehlgeschlagen"; detail };
    model
  | LoadInvoice i ->
    let b =
      match List.nth model.local.bookings i with
      | None -> fresh_booking ()
      | Some b -> b
    in
    let inv = Invoice.of_customer_and_booking (Ext_date.today ()) model.local b in
    { model with invoice_form = Invoice_form.Model.load inv; view = Invoice i }
  | InvoiceForm a ->
    let schedule_action = Fn.compose schedule_action Action.invoiceform in
    let invoice_form = Component.apply_action ~schedule_action invoice a state in
    { model with invoice_form }
  | NavChange (Id i) ->
    let rq =
      Request.map_resp ~f:(fun c -> i, c) Remote.(Customer.get i |> finalize conn)
    in
    let handler = Fn.compose schedule_action Action.gotcustomer in
    Request.XHR.send' ~handler rq;
    Model.create_loading (Id i) ()
  | NavChange New -> Model.create ()
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

let input_bool state label id =
  let classes, divs =
    match err_of_block state id with
    | None -> [], []
    | Some m -> [ "is-invalid" ], [ Node.div [ Attr.class_ "invalid-feedback" ] [ m ] ]
  in
  Node.div
    [ Attr.classes [ "custom-control"; "custom-checkbox" ] ]
    ([ Form.Input.checkbox state id [ Attr.classes ("custom-control-input" :: classes) ]
     ; Form.Input.label id [ Attr.class_ "custom-control-label" ] [ Node.text label ]
     ]
    @ divs)
;;

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

let input_number ~step =
  input_str ~attr:[ Attr.create "step" (string_of_float step) ] ~type_:"number"
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

let view_period state ids =
  let from, till = ids in
  Bs.Grid.(
    frow
      [ col [ input_str state "Von" ~type_:"date" from ]
      ; col [ input_str state "Bis" ~type_:"date" till ]
      ])
;;

let view_delete_button action title =
  Bs.button
    ~i:(R "trash-alt")
    ~style:"outline-danger"
    ~attr:[ Bs.tab_skip ]
    ~action
    title
;;

let dl_id = id ()

let view_alloc delete state ids =
  let block, (room, (price_per_bed, (beds, (description, ())))) = ids in
  (* This puts redundant datalist with same id into the form. One for each room
     Since datalists are static, it is no problem. *)
  let datalist = dl_id, Booking.room_descriptions in
  Bs.Grid.
    [ frow [ col2 (prepend_err_div state block []) ]
    ; frow
        [ col3 [ input_str state "Nr." room ]
        ; col9 [ input_str ~datalist state "Beschreibung" description ]
        ]
    ; frow
        [ col4 [ input_number ~step:1. state "Betten" beds ]
        ; col4 [ input_number ~step:0.01 state "Preis" price_per_bed ]
        ; col4
            ~c:[ "align-self-end"; "text-right" ]
            [ view_delete_button delete "Zimmer löschen" ]
        ]
    ]
;;

let view_guest delete state ids =
  let block, (given, (family, (born, ()))) = ids in
  Bs.Grid.
    [ frow [ col (prepend_err_div state block []) ]
    ; frow
        [ col [ input_str state "Vorname(n)" given ]
        ; col [ input_str state "Nachname" family ]
        ]
    ; frow
        [ col [ input_str state "Geburtsdatum" ~type_:"date" born ]
        ; col
            ~c:[ "align-self-end"; "text-right" ]
            [ view_delete_button delete "Gast löschen" ]
        ]
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

let excel_id = id ()

let uri_of_letter customer t =
  let date = Browser.Date.(now () |> to_locale_date_string) in
  Letter.(t ~date customer |> href)
;;

let view_booking ~inject ~sync (customer : Customer.t) selected state ids =
  let ( block
      , ( period
        , ( deposit_asked
          , (deposit_got, (tax_free, (note, ((guests, g_lst), ((positions, p_lst), ())))))
          ) ) )
    =
    ids
  in
  let new_ lst_id _ev =
    let state = Form.List.append state lst_id in
    inject (Action.FormUpdate state)
  in
  let delete lst_id i _ev =
    let state = Form.List.remove_nth state lst_id i in
    inject (Action.FormUpdate state)
  in
  let new_g = new_ g_lst
  and new_p = new_ p_lst
  and new_b _evt = inject Action.NewBooking
  and delete_g = delete g_lst
  and delete_p = delete p_lst in
  let guests =
    (Node.h4 [] [ Node.text "Gäste" ]
    :: List.concat_mapi guests ~f:(fun i ids ->
           Node.hr [] :: view_guest (delete_g i) state ids))
    @ [ Node.hr []
      ; Node.div [] [ Bs.button ~i:(S "plus") ~action:new_g "Weiterer Gast" ]
      ]
  and allocs =
    (Node.h4 [] [ Node.text "Positionen" ]
    :: List.concat_mapi positions ~f:(fun i ids ->
           Node.hr [] :: view_alloc (delete_p i) state ids))
    @ [ Node.hr []
      ; Node.div [] [ Bs.button ~i:(S "plus") ~action:new_p "Weitere Position" ]
      ]
  and main =
    Bs.Grid.
      [ Node.h4 [] [ Node.text "Aufenthalt" ]
      ; Node.hr []
      ; div [] (prepend_err_div state block [])
      ; view_period state period
      ; frow
          [ col [ input_number ~step:0.01 state "Anzahlung gefordert" deposit_asked ]
          ; col [ input_number ~step:0.01 state "Anzahlung erhalten" deposit_got ]
          ]
      ; frow [ col ~c:[ "mb-2" ] [ input_bool state "befreit von Kurtaxe" tax_free ] ]
      ; frow [ col [ input_str ~input:(textarea 8) state "Notiz" note ] ]
      ]
  in
  let excel =
    match List.nth customer.bookings selected with
    | None -> ""
    | Some b -> Excel_br_2014_v2.of_customer_and_booking customer b
  and confirmation =
    match List.nth customer.bookings selected with
    | None -> ""
    | Some booking -> uri_of_letter customer (Letter.confirm ~booking)
  and delete_b _evt = inject (Action.DeleteBooking selected)
  and invoice_btn =
    Bs.button
      ~style:"info"
      ~action:(fun _ -> inject (Action.LoadInvoice selected))
      "Test: Rechnung"
  in
  Bs.Grid.
    [ row [ col main; col allocs; col guests ]
    ; frow
        [ col_auto
            ~c:[ "mb-2"; "mt-2" ]
            [ Bs.button' ~href:confirmation ~blank:true "Bestätigung" ]
        ; col_auto
            ~c:[ "mb-2"; "mt-2" ]
            [ Bs.button_clipboard ~value:excel ~id:excel_id "Excel" ]
        ; col_auto ~c:[ "mb-2"; "mt-2" ] [ invoice_btn ]
        ; col
            [ frow
                ~c:[ "justify-content-end" ]
                [ col_auto
                    ~c:[ "mb-2"; "mt-2" ]
                    [ danger_btn delete_b "Buchung löschen" ]
                ; col_auto
                    ~c:[ "mb-2"; "mt-2" ]
                    [ Bs.button ~action:new_b "Neue Buchung" ]
                ; col_auto ~c:[ "mb-2"; "mt-2" ] [ save_btn ~inject ~sync ]
                ]
            ]
        ]
    ]
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

let view_form ~invoice (model : Model.t Incr.t) ~inject =
  let open Vdom in
  let%map form = model >>| Model.form
  and local = model >>| Model.local
  and sync = model >>| Model.sync
  and invoice = invoice
  and view = model >>| Model.view in
  let ids = Form.State.field_ids form customer_form in
  let save _evt = inject Action.Save in
  let show visible =
    let c = if visible then [] else [ Attr.class_ "d-none" ] in
    Node.div c
  in
  let rows =
    (view_main ~inject ~sync local form ids |> show (view = Main))
    :: List.concat_mapi
         ~f:(fun i b_ids ->
           [ view_booking ~inject ~sync local i form b_ids |> show (view = Booking i)
           ; [ Component.view invoice ] |> show (view = Invoice i)
           ])
         (booking_entry_ids form)
  in
  let touch _ _ = inject Action.Touch in
  Node.create "form" [ Attr.on "submit" save; Attr.on_input touch ] rows
;;

let view ~invoice ~inject model =
  let%bind loading = model >>| Model.loading in
  if loading then Incr.const Bs.Grid.loading_row else view_form ~invoice ~inject model
;;

let menu ~inject (m : Model.t) : Menu.t =
  let open Menu in
  let goto_main = On_click (fun () -> inject (Action.ChangeView Main)) in
  let children =
    let f i v =
      let title =
        match v with
        | Model.Fresh -> "Neue Buchung"
        | Saved p ->
          let open Period in
          let f, t = Localize.date (from p), Localize.date (till p) in
          sprintf "%s bis %s" f t
      and action = Menu.On_click (fun () -> inject (Action.ChangeView (Booking i))) in
      entry title action (m.view = Model.Booking i)
    in
    entry "Stammdaten" goto_main (m.view = Main) :: List.mapi ~f m.bookings
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
  let invoice =
    let inject = Fn.compose inject Action.invoiceform
    and back_href = Nav.(href_of Overview)
    and form_model = model >>| Model.invoice_form in
    Invoice_form.create ~inject ~back_href form_model
  in
  let%map model = model
  and invoice = invoice
  and view = view ~invoice ~inject model in
  let apply_action = apply_action ~invoice model
  and extra : Menu.t = menu ~inject model in
  Component.create_with_extra ~apply_action ~extra model view
;;
