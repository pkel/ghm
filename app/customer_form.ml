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

let name_descr =
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
      if List.is_empty errors then Ok t else Error errors )
;;

let company_descr =
  let open Form.Description in
  let unvalidated =
    let open Of_record in
    build_for_record
      (Customer.Company.Fields.make_creator ~name:(field string) ~address:(field string))
  in
  conv unvalidated ~f:(fun t _ ~block_id:_ ->
      let errors = [] in
      if List.is_empty errors then Ok t else Error errors )
;;

let address_descr =
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
      if List.is_empty errors then Ok t else Error errors )
;;

let contact_descr =
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
      if List.is_empty errors then Ok t else Error errors )
;;

let nonempty_string errmsg =
  let open Form.Description in
  conv_without_block string ~f:(fun s id ->
      match s with
      | "" -> Error [Form.Form_error.create ~id (Error.of_string errmsg)]
      | s -> Ok s )
;;

let opt_date =
  let open Form.Description in
  conv_without_block string ~f:(fun s id ->
      if s = ""
      then Ok None
      else
        match Date.of_string s with
        | d -> Ok (Some d)
        | exception _ ->
          Error [Form.Form_error.create ~id (Error.of_string "Datum nicht auswertbar.")]
  )
  |> contra_map ~f:(function None -> "" | Some d -> Date.to_string d)
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
      Ok (Period.of_dates f t) )
;;

let monetary_opt =
  let open Form.Description in
  conv_without_block string ~f:(fun s id ->
      if s = ""
      then Ok None
      else
        match float_of_string s with
        | x -> Ok (Some (Float.round_decimal ~decimal_digits:2 x))
        | exception _ ->
          Error [Form.Form_error.create ~id (Error.of_string "Geldwert erwartet.")] )
  |> contra_map ~f:(function None -> "" | Some x -> sprintf "%.2f" x)
;;

let monetary =
  let open Form.Description in
  conv_without_block string ~f:(fun s id ->
      if s = ""
      then Ok 0.
      else
        match float_of_string s with
        | x -> Ok (Float.round_decimal ~decimal_digits:2 x)
        | exception _ ->
          Error [Form.Form_error.create ~id (Error.of_string "Geldwert erwartet.")] )
  |> contra_map ~f:(function 0. -> "" | x -> sprintf "%.2f" x)
;;

let int =
  let open Form.Description in
  conv_without_block string ~f:(fun s id ->
      if s = ""
      then Ok 0
      else
        match int_of_string s with
        | i -> Ok i
        | exception _ ->
          Error [Form.Form_error.create ~id (Error.of_string "Ganze Zahl erwartet.")] )
  |> contra_map ~f:(function 0 -> "" | i -> string_of_int i)
;;

let customer_descr =
  let open Form.Description in
  let unvalidated =
    let open Of_record in
    build_for_record
      (Customer.Fields.make_creator
         ~name:(field name_descr)
         ~address:(field address_descr)
         ~company:(field company_descr)
         ~contact:(field contact_descr)
         ~keyword:(field (nonempty_string "Schlüsselwort darf nicht leer sein"))
         ~note:(field string)
         ~bookings:(field (not_editable ~default:[])))
  in
  conv unvalidated ~f:(fun t _ ~block_id:_ -> Ok t)
;;

let guest_descr =
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

let alloc_descr =
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

let booking_descr =
  let open Form.Description in
  let unvalidated =
    Of_record.(
      build_for_record
        (Booking.Fields.make_creator
           ~deposit_asked:(field monetary_opt)
           ~deposit_got:(field monetary_opt)
           ~tax_free:(field bool)
           ~note:(field string)
           ~guests:(field (list guest_descr))
           ~allocs:(field (list alloc_descr))
           ~period:(field period)))
  in
  conv unvalidated ~f:(fun b _ ~block_id:_ -> Ok b)
;;

let customer_form = Form.create ~name:"customer form" customer_descr
let booking_form = Form.create ~name:"booking form" booking_descr

module Model = struct
  type t =
    { customer_f : Form.State.t
    ; booking_f :
        Form.State.t
        (* TODO: use one form states only, avoid having the copy of customer here
             a list of form states might do for the bookings. *)
    ; customer : Customer.t
    ; nav : Nav.customer
    ; selected : int }
  [@@deriving compare, fields]

  let load nav (c : Customer.t) =
    let booking = match c.bookings with [] -> None | hd :: _ -> Some hd in
    { customer_f = Form.State.create ~init:c customer_form
    ; booking_f = Form.State.create ?init:booking booking_form
    ; customer = c
    ; nav
    ; selected = 0 }
  ;;

  let create () = load New Customer.empty
end

module Action = struct
  type t =
    | Update_c of Form.State.t sexp_opaque
    | Update_b of Form.State.t sexp_opaque
    | SelectBooking of int
    | NavChange of Nav.customer sexp_opaque
    | GotCustomer of (int * Customer.t) Or_error.t
    | Save
    | NewBooking
    | DeleteBooking
    | DeleteCustomer
  [@@deriving sexp, variants]
end

module State = struct
  type t = unit
end

let default_period () =
  let today = Ext_date.today () in
  let from = Date.add_days today 1
  and till = Date.add_days today 2 in
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
    ; allocs = [] }
;;

let apply_action
    (model : Model.t) (action : Action.t) (_state : State.t) ~schedule_action : Model.t =
  match action with
  | Update_c customer_f ->
    Log.form customer_f;
    {model with customer_f}
  | Update_b booking_f ->
    Log.form booking_f;
    {model with booking_f}
  | SelectBooking selected ->
    let b_opt, booking_f = Form.State.read_value model.booking_f booking_form in
    (match b_opt with
    | None -> {model with booking_f}
    | Some b ->
      let bookings =
        List.mapi model.customer.bookings ~f:(fun i old ->
            if i = model.selected then b else old )
      and booking_f =
        let init = List.nth model.customer.bookings selected in
        Form.State.create ?init booking_form
      in
      let customer = {model.customer with bookings} in
      {model with selected; customer; booking_f})
  | Save ->
    let c_opt, customer_f = Form.State.read_value model.customer_f customer_form
    and b_opt, booking_f = Form.State.read_value model.booking_f booking_form in
    schedule_action (Action.Update_c customer_f);
    schedule_action (Action.Update_b booking_f);
    let bookings =
      match b_opt with
      | None -> model.customer.bookings
      | Some b ->
        List.mapi model.customer.bookings ~f:(fun i old ->
            if i = model.selected then b else old )
    in
    (match c_opt with
    | None -> model
    | Some c ->
      let c = {c with bookings} in
      let () =
        match model.nav with
        | New ->
          let handler = Fn.compose schedule_action Action.gotcustomer in
          Request.XHR.send ~body:c ~handler Remote.Customer.post
        | Id id ->
          let rq = Request.map_resp ~f:(fun c -> id, c) (Remote.Customer.patch id) in
          let handler = Fn.compose schedule_action Action.gotcustomer in
          Request.XHR.send ~body:c ~handler rq
      in
      model)
  | NewBooking ->
    let b_opt, booking_f = Form.State.read_value model.booking_f booking_form in
    schedule_action (Action.Update_b booking_f);
    (match b_opt with
    | None -> (* booking form blocked by error *) model
    | Some b ->
      (* save current booking, create new, reload *)
      let new_b = fresh_booking () in
      (* TODO: copy latest booking with new date, currently a bug is doing it *)
      let booking_f = Form.State.create ~init:new_b booking_form in
      let bookings =
        new_b
        :: List.mapi model.customer.bookings ~f:(fun i old ->
               if i = model.selected then b else old )
      in
      {model with customer = {model.customer with bookings}; selected = 0; booking_f})
  | DeleteBooking ->
    let bookings =
      List.filteri ~f:(fun i _ -> i <> model.selected) model.customer.bookings
    in
    let selected = min model.selected (List.length bookings - 1) in
    let booking_f = Form.State.create booking_form ?init:(List.nth bookings selected) in
    {model with customer = {model.customer with bookings}; selected; booking_f}
  | DeleteCustomer ->
    let () =
      match model.nav with
      | New -> Nav.(set Overview)
      | Id i ->
        let handler = function Error e -> Log.error e | Ok () -> Nav.(set Overview) in
        Request.XHR.send' ~handler (Remote.Customer.delete i)
    in
    model
  | GotCustomer response ->
    (match response with
    | Ok (id, c) -> Model.load (Id id) c
    | Error e ->
      Log.error e;
      model (* TODO: display this to user, ideally don't show form without data. *))
  | NavChange n ->
    (match n with
    | Id i ->
      let rq = Request.map_resp ~f:(fun c -> i, c) (Remote.Customer.get i) in
      let handler = Fn.compose schedule_action Action.gotcustomer in
      Request.XHR.send' ~handler rq;
      Model.create ()
    | New -> Model.create ())
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
      Node.div [Attr.classes ["alert"; "alert-danger"]] [x] )
;;

let prepend_err_div state (id : Form.Block.t Form.Id.t) rows =
  match err_div_of_block state id with Some x -> x :: rows | None -> rows
;;

let group = Node.div [Attr.class_ "form-group"]

let input_bool state label id =
  let classes, divs =
    match err_of_block state id with
    | None -> [], []
    | Some m -> ["is-invalid"], [Node.div [Attr.class_ "invalid-feedback"] [m]]
  in
  Node.div
    [Attr.classes ["custom-control"; "custom-checkbox"]]
    ( [ Form.Input.checkbox state id [Attr.classes ("custom-control-input" :: classes)]
      ; Form.Input.label id [Attr.class_ "custom-control-label"] [Node.text label] ]
    @ divs )
;;

let input_str
    ?(attr = []) ?(input = Form.Input.text) ?(type_ = "text") ?datalist state label id =
  let classes, error =
    match err_of_block state id with
    | None -> [], []
    | Some m -> ["is-invalid"], [Node.div [Attr.class_ "invalid-feedback"] [m]]
  in
  let attr, datalist =
    match datalist with
    | None -> attr, []
    | Some (id, l) ->
      ( Attr.create "list" id :: attr
      , [ Node.create
            "datalist"
            [Attr.id id]
            (List.map l ~f:(fun s -> Node.create "option" [Attr.value s] [])) ] )
  in
  group
    ( [ Form.Input.label id [] [Node.text label]
      ; input
          state
          id
          (Attr.classes ("form-control" :: classes) :: Attr.type_ type_ :: attr) ]
    @ error
    @ datalist )
;;

let input_number ~step =
  input_str ~attr:[Attr.create "step" (string_of_float step)] ~type_:"number"
;;

let dl1_id = id ()
let dl2_id = id ()

let view_name state ids =
  let block, (title, (letter, (given, (family, ())))) = ids in
  let titles, letters = List.unzip Customer.letter_by_title in
  let titles = dl1_id, titles in
  let letters = dl2_id, letters in
  Bs.Grid.
    [ frow [col (prepend_err_div state block [])]
    ; frow
        [ col4 [input_str ~datalist:titles state "Titel" title]
        ; col8 [input_str ~datalist:letters state "Anrede Brief" letter] ]
    ; frow
        [col [input_str state "Vorname" given]; col [input_str state "Nachname" family]]
    ]
;;

let view_company state ids =
  let block, (name, (address, ())) = ids in
  Bs.Grid.
    [ frow [col (prepend_err_div state block [])]
    ; frow [col [input_str state "Firma" name]]
    ; frow [col [input_str state "Abteilung" address]] ]
;;

let view_address state ids =
  let block, (street_with_num, (postal_code, (city, (country, (country_code, ()))))) =
    ids
  in
  Bs.Grid.
    [ frow [col (prepend_err_div state block [])]
    ; frow [col [input_str state "Straße und Hausnummer" street_with_num]]
    ; frow
        [ col4 [input_str state "Postleitzahl" postal_code]
        ; col8 [input_str state "Ort" city] ]
    ; frow
        [ col8 [input_str state "Land" country]
        ; col4 [input_str state "Code" country_code] ] ]
;;

let view_contact state ids =
  let block, (phone, (phone2, (mobile, (fax, (fax2, (mail, (mail2, (web, ())))))))) =
    ids
  in
  Bs.Grid.
    [ frow [col (prepend_err_div state block [])]
    ; frow [col [input_str state "Telefon" phone]; col [input_str state "Telefon" phone2]]
    ; frow [col [input_str state "Mobil" mobile]]
    ; frow [col [input_str state "Fax" fax]; col [input_str state "Fax" fax2]]
    ; frow [col [input_str state "Mail" mail]]
    ; frow [col [input_str state "Mail" mail2]]
    ; frow [col [input_str state "Internet" web]] ]
;;

let view_period state ids =
  let from, till = ids in
  Bs.Grid.(
    frow
      [ col [input_str state "Von" ~type_:"date" from]
      ; col [input_str state "Bis" ~type_:"date" till] ])
;;

let view_delete_button action title =
  Bs.button ~i:(R "trash-alt") ~style:"outline-danger" ~attr:[Bs.tab_skip] ~action title
;;

let dl_id = id ()

let view_alloc delete state ids =
  let block, (room, (price_per_bed, (beds, (description, ())))) = ids in
  (* This puts redundant datalist with same id into the form. One for each room
     Since datalists are static, it is no problem. *)
  let datalist = dl_id, Booking.room_descriptions in
  Bs.Grid.
    [ frow [col2 (prepend_err_div state block [])]
    ; frow
        [ col3 [input_str state "Nr." room]
        ; col9 [input_str ~datalist state "Beschreibung" description] ]
    ; frow
        [ col4 [input_number ~step:1. state "Betten" beds]
        ; col4 [input_number ~step:0.01 state "Preis" price_per_bed]
        ; col4
            ~c:["align-self-end"; "text-right"]
            [view_delete_button delete "Zimmer löschen"] ] ]
;;

let view_guest delete state ids =
  let block, (given, (family, (born, ()))) = ids in
  Bs.Grid.
    [ frow [col (prepend_err_div state block [])]
    ; frow
        [ col [input_str state "Vorname(n)" given]
        ; col [input_str state "Nachname" family] ]
    ; frow
        [ col [input_str state "Geburtsdatum" ~type_:"date" born]
        ; col
            ~c:["align-self-end"; "text-right"]
            [view_delete_button delete "Gast löschen"] ] ]
;;

let textarea n state id attr =
  Form.Input.textarea state id (Attr.create "rows" (string_of_int n) :: attr)
;;

let view_booking ~inject selection state ids =
  let ( block
      , ( period
        , ( deposit_asked
          , (deposit_got, (tax_free, (note, ((guests, g_lst), ((allocs, a_lst), ()))))) )
        ) ) =
    ids
  in
  let new_ lst_id _ev =
    let list_adder =
      { Form.List.transform =
          (* TODO: Prefill *)
          (fun list ~create_new_form -> list @ [create_new_form ()]) }
    in
    let state = Form.List.modify_list state lst_id ~f:list_adder in
    inject (Action.Update_b state)
  in
  let delete lst_id i _ev =
    let state = Form.List.remove_nth state lst_id i in
    inject (Action.Update_b state)
  in
  let new_g = new_ g_lst
  and new_a = new_ a_lst
  and delete_g = delete g_lst
  and delete_a = delete a_lst in
  let guests =
    ( Node.h4 [] [Node.text "Gäste"]
    :: List.concat_mapi guests ~f:(fun i ids ->
           Node.hr [] :: view_guest (delete_g i) state ids ) )
    @ [Node.hr []; Node.div [] [Bs.button ~i:(S "plus") ~action:new_g "Weiterer Gast"]]
  and allocs =
    ( Node.h4 [] [Node.text "Belegung"]
    :: List.concat_mapi allocs ~f:(fun i ids ->
           Node.hr [] :: view_alloc (delete_a i) state ids ) )
    @ [Node.hr []; Node.div [] [Bs.button ~i:(S "plus") ~action:new_a "Weitere Belegung"]]
  and main =
    Bs.Grid.
      [ Node.h4 [A.class_ "mb-3"] [Node.text "Buchungen"]
      ; selection
      ; div [] (prepend_err_div state block [])
      ; view_period state period
      ; frow
          [ col [input_number ~step:0.01 state "Anzahlung gefordert" deposit_asked]
          ; col [input_number ~step:0.01 state "Anzahlung erhalten" deposit_got] ]
      ; frow [col ~c:["mb-2"] [input_bool state "befreit von Kurtaxe" tax_free]]
      ; frow [col [input_str ~input:(textarea 8) state "Notiz" note]] ]
  in
  Bs.Grid.(row [col main; col allocs; col guests])
;;

let view_booking_list ~selected ~inject (l : Booking.t list) : Vdom.Node.t =
  let f i b =
    let f, t =
      let p = Booking.period b in
      let open Period in
      Localize.date (from p), Localize.date (till p)
    in
    let e =
      Attr.
        [ on_click (fun _ -> inject (Action.SelectBooking i))
        ; style (Css.create ~field:"cursor" ~value:"pointer") ]
    in
    let attr = if i = selected then Attr.class_ "table-active" :: e else e in
    Node.(tr attr [td [] [text f]; td [] [text t]])
  in
  Node.(
    table
      [Attr.classes ["table"; "table-sm"; "table-hover"]]
      [ thead [] [tr [] [th [] [text "von"]; th [] [text "bis"]]]
      ; tbody [] (List.mapi ~f l) ])
;;

let view_customer state ids =
  let block, (name, (company, (address, (contact, (keyword, (note, ((), ()))))))) =
    ids
  in
  let left =
    Bs.Grid.(
      (frow [col [input_str state "Schlüsselwort" keyword]] :: view_name state name)
      @ [frow [col [input_str ~input:(textarea 8) state "Notiz" note]]])
  and middle = view_address state address @ view_company state company
  and right = view_contact state contact in
  Bs.Grid.[row (prepend_err_div state block []); row [col left; col middle; col right]]
;;

let letter_dropdown_id = id ()
let excel_id = id ()

let view (model : Model.t Incr.t) ~back_href ~inject =
  let open Vdom in
  let%map customer_f = model >>| Model.customer_f
  and booking_f = model >>| Model.booking_f
  and customer = model >>| Model.customer
  and bookings = model >>| Model.customer >>| Customer.bookings
  and selected = model >>| Model.selected in
  let excel =
    match List.nth bookings selected with
    | None -> ""
    | Some b -> Excel_br_2014_v2.of_customer_and_booking customer b
  in
  let c_ids = Form.State.field_ids customer_f customer_form
  and b_ids = Form.State.field_ids booking_f booking_form in
  let save _evt = inject Action.Save
  and new_b _evt = inject Action.NewBooking
  and delete_b _evt = inject Action.DeleteBooking
  and delete_c _evt = inject Action.DeleteCustomer
  and selection = view_booking_list ~inject bookings ~selected
  and danger_btn action title =
    Bs.button ~attr:[Bs.tab_skip] ~style:"outline-danger" ~action title
  and letter_dropdown =
    let items =
      let f x =
        let uri t =
          let date = Browser.Date.(now () |> to_locale_date_string)
          and sender = "Pension Keller, Am Vögelisberg 13, D-78479 Reichenau"
          and signer = "Christine Keller" in
          "/letter/#" ^ Letter.(t ~sender ~signer ~date customer |> to_b64)
        in
        Node.a
          Attr.[class_ "dropdown-item"; href (uri (snd x)); create "target" "_blank"]
          [Node.text (fst x)]
      and letters =
        let open Letter in
        [ "Leere Vorlage", blank ~attachments:[]
        ; "Leere Vorlage (mit Anhang)", blank ~attachments:["Anhang 1"; "Anhang 2"]
        ; "Hausprospekt", flyer ]
        @
        match List.nth bookings selected with
        | None -> []
        | Some booking -> ["Bestätigung", confirm ~booking]
      in
      List.map ~f letters
    in
    Node.(
      div
        [Attr.classes ["dropdown"; "dropup"]]
        [ a
            Attr.
              [ classes ["btn"; "btn-secondary"; "dropdown-toggle"]
              ; href "#"
              ; type_ "button"
              ; id letter_dropdown_id
              ; create "data-toggle" "dropdown"
              ; create "aria-haspopup" "true"
              ; create "aria-expanded" "false" ]
            [Node.text "Brief"]
        ; div
            Attr.[class_ "dropdown-menu"; create "aria-labelledby" letter_dropdown_id]
            items ])
  in
  let rows =
    let open Bs.Grid in
    [ [ frow
          [ col_auto ~c:["mt-2"] [Bs.button' ~href:back_href "Zurück"]
          ; col
              [ frow
                  ~c:["justify-content-end"]
                  [col_auto ~c:["mb-2"; "mt-2"] [Bs.button_submit "Speichern"]] ] ]
      ; Node.hr [] ]
    ; view_customer customer_f c_ids
    ; [ Node.hr []
      ; view_booking ~inject selection booking_f b_ids
      ; frow
          [ col_auto
              ~c:["mb-2"; "mt-2"]
              [Bs.button_clipboard ~value:excel ~id:excel_id "Excel"]
          ; col_auto ~c:["mb-2"; "mt-2"] [letter_dropdown]
          ; col
              [ frow
                  ~c:["justify-content-end"]
                  [ col_auto ~c:["mb-2"; "mt-2"] [danger_btn delete_b "Buchung löschen"]
                  ; col_auto ~c:["mb-2"; "mt-2"] [danger_btn delete_c "Kunde löschen"]
                  ; col_auto ~c:["mb-2"; "mt-2"] [Bs.button ~action:new_b "Neue Buchung"]
                  ] ] ] ] ]
  in
  Node.create "form" [Attr.on "submit" save] (List.concat rows)
;;

let create
    ~(back_href : string) ~(inject : Action.t -> Vdom.Event.t) (model : Model.t Incr.t) =
  let%map model = model
  and view = view ~inject ~back_href model in
  let apply_action = apply_action model in
  Component.create ~apply_action model view
;;
