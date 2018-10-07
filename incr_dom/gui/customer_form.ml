open Core_kernel
open Ghm
open Incr_dom
open Incr_dom_widgets
open Incr.Let_syntax

let name_descr =
  let open Form.Description in
  let unvalidated =
    let open Of_record in
    build_for_record (
      Customer.Name.Fields.make_creator
        ~title:(field string)
        ~given:(field string)
        ~second:(field string)
        ~family:(field string))
  in conv unvalidated
      ~f:(fun t _ ~block_id:_ ->
          let errors = [] in
          if List.is_empty errors
          then (Ok t)
          else (Error errors)
        )

let company_descr =
  let open Form.Description in
  let unvalidated =
    let open Of_record in
    build_for_record (
      Customer.Company.Fields.make_creator
        ~name:(field string)
        ~address:(field string))
  in conv unvalidated
      ~f:(fun t _ ~block_id:_ ->
          let errors = [] in
          if List.is_empty errors
          then (Ok t)
          else (Error errors)
        )

let address_descr =
  let open Form.Description in
  let unvalidated =
    let open Of_record in
    build_for_record (
      Customer.Address.Fields.make_creator
        ~street:(field string)
        ~street_number:(field string)
        ~postal_code:(field string)
        ~city:(field string)
        ~country:(field string)
        ~country_code:(field string))
  in conv unvalidated
      ~f:(fun t _ ~block_id:_ ->
          let errors = [] in
          if List.is_empty errors
          then (Ok t)
          else (Error errors)
        )

let contact_descr =
  let open Form.Description in
  let unvalidated =
    let open Of_record in
    build_for_record (
      Customer.Contact.Fields.make_creator
        ~phone:(field string)
        ~phone2:(field string)
        ~mobile:(field string)
        ~fax:(field string)
        ~fax2:(field string)
        ~mail:(field string)
        ~mail2:(field string)
        ~web:(field string))
  in conv unvalidated
    ~f:(fun t _ ~block_id:_ ->
        let errors = [] in
        if List.is_empty errors
        then (Ok t)
        else (Error errors)
      )

let nonempty_string errmsg =
  let open Form.Description in
  conv_without_block string ~f:(fun s id -> match s with
      | "" -> Error [Form.Form_error.create ~id (Error.of_string errmsg)]
      | s -> Ok s)

let opt_date =
  let open Form.Description in
  conv_without_block string ~f:(fun s id ->
      if s = "" then Ok (None) else
        match Date.of_string s with
        | d -> Ok (Some d)
        | exception _ ->
          Error [Form.Form_error.create ~id (
              Error.of_string "Datum nicht auswertbar.")])
  |> contra_map ~f:(function
      | None -> ""
      | Some d -> Date.to_string d)

let period =
  let open Form.Description in
  let unvalidated =
    let open Let_syntax in
    let%map_open from = opt_date <^ (Fn.compose Option.some Period.from)
    and till = opt_date <^ (Fn.compose Option.some Period.till) in
    from, till
  in
  conv_without_block unvalidated ~f:(fun (f, t) _ ->
      let default =
        Date.(add_days (today ~zone:Time.Zone.utc) 7)
      in let f, t =
           match f, t with
           | None, None -> default, Date.add_days default 1
           | None, Some d
           | Some d, None -> d, Date.add_days d 1
           | Some a, Some b -> a, b
      in Ok (Period.of_dates f t))

let monetary_opt =
  let open Form.Description in
  conv_without_block string ~f:(fun s id ->
      if s = "" then Ok None else
        match float_of_string s with
        | x -> Ok (Some (Float.round_decimal ~decimal_digits: 2 x))
        | exception _ ->
          Error [Form.Form_error.create ~id (
              Error.of_string "Geldwert erwartet.")])
  |> contra_map ~f:(function
      | None -> ""
      | Some x -> string_of_float x)

let monetary =
  let open Form.Description in
  conv_without_block string ~f:(fun s id ->
      if s = "" then Ok 0. else
        match float_of_string s with
        | x -> Ok (Float.round_decimal ~decimal_digits: 2 x)
        | exception _ ->
          Error [Form.Form_error.create ~id (
              Error.of_string "Geldwert erwartet.")])
  |> contra_map ~f:(function
      | 0. -> ""
      | x -> string_of_float x)

let int =
  let open Form.Description in
  conv_without_block string ~f:(fun s id ->
      if s = "" then Ok 0 else
        match int_of_string s with
        | i -> Ok i
        | exception _ ->
          Error [Form.Form_error.create ~id (
              Error.of_string "Ganze Zahl erwartet.")])
  |> contra_map ~f:(function
      | 0 -> ""
      | i -> string_of_int i)

let percent =
  let open Form.Description in
  map int ~f:(fun x -> float_of_int x /. 100.)
  |> contra_map ~f:(fun x -> Float.round_down (x *. 100.) |> int_of_float )

let customer_descr =
  let open Form.Description in
  let unvalidated =
    let open Of_record in
    build_for_record (
      Customer.Fields.make_creator
        ~name:(field name_descr)
        ~address:(field address_descr)
        ~company:(field company_descr)
        ~contact:(field contact_descr)
        ~keyword:(field (nonempty_string "Schlüsselwort darf nicht leer sein"))
        ~note:(field string)
        ~bookings:(field (not_editable ~default:[]))
    )
  in
  conv unvalidated ~f:(fun t _ ~block_id:_ -> Ok t)

let guest_descr =
  let open Form.Description in
  let unvalidated =
    Of_record.(build_for_record (
        Booking.Fields_of_guest.make_creator
          ~given:(field string)
          ~second:(field string)
          ~family:(field string)
          ~born:(field opt_date)))
  in conv unvalidated ~f:(fun t _ ~block_id:_ -> Ok t)

let room_descr =
  let open Form.Description in
  let unvalidated =
    Of_record.(build_for_record (
        Booking.Fields_of_room.make_creator
          ~room:(field string)
          ~beds:(field int)
          ~price_per_bed:(field monetary)
          ~factor:(field percent)
          ~description:(field string)
          ~period:(field period)))
  in conv unvalidated ~f:(fun t _ ~block_id:_ -> Ok t)

let booking_descr =
  let open Form.Description in
  let unvalidated =
    Of_record.(build_for_record (
        Booking.Fields.make_creator
          ~deposit_asked:(field monetary_opt)
          ~deposit_got:(field monetary_opt)
          ~no_tax:(field bool)
          ~note:(field string)
          ~guests:(field (list guest_descr))
          ~rooms:(field (list room_descr))))
  in conv unvalidated ~f:(fun b _ ~block_id:_ -> Ok b)

let customer_form = Form.create ~name:"customer form" customer_descr
let booking_form = Form.create ~name:"booking form" booking_descr

module Model = struct
  type t =
    { customer : Form.State.t
    ; booking : Form.State.t
    ; bookings : Booking.t list
    ; selected : int
    } [@@deriving compare, fields]

  let load (c: Customer.t) =
    let booking = match c.bookings with
      | [] -> None
      | hd :: _ -> Some hd
    in
    { customer = Form.State.create ~init:c customer_form
    ; booking = Form.State.create ?init:booking booking_form
    ; bookings = c.bookings
    ; selected = 0 }

  let empty () = load Customer.empty
end

module Action = struct
  type t =
    | UpdateCustomer of Form.State.t sexp_opaque
    | SelectBooking of int
  [@@deriving sexp]
end

module State = struct type t = unit end

let apply_action (model: Model.t) (action: Action.t) (_state: State.t)
    ~schedule_action:_ : Model.t =
  match action with
  | UpdateCustomer customer -> Log.form customer; { model with customer }
  | SelectBooking selected -> { model with selected }

open Vdom

let err_of_block state id =
  let msg err =
    String.strip ~drop:(fun c -> Char.equal c '"') (Error.to_string_hum err)
  in Option.map (Form.State.error state id) ~f:(fun x -> Node.text (msg x))

let err_div_of_block state id =
  Option.map (err_of_block state id) ~f:(fun x ->
      Node.div [Attr.classes ["alert"; "alert-danger"]] [ x ])

let prepend_err_div state (id: Form.Block.t Form.Id.t) rows =
  match err_div_of_block state id with
  | Some x -> x :: rows
  | None -> rows

let group = Node.div [ Attr.class_ "form-group" ]

let input_bool state label id =
  let classes, divs =
    match err_of_block state id with
    | None -> [], []
    | Some m -> ["is-invalid"], [Node.div [Attr.class_ "invalid-feedback"] [m]]
  in
  group (
       [ Node.label [] [Node.text label]
       ; Form.Input.checkbox state id [Attr.classes ("form-control" :: classes)]
       ] @ divs)

let input_str ?(input=Form.Input.text) state label id =
  let classes, divs =
    match err_of_block state id with
    | None -> [], []
    | Some m -> ["is-invalid"], [Node.div [Attr.class_ "invalid-feedback"] [m]]
  in
  group (
       [ Node.label [] [Node.text label]
       ; input state id [Attr.classes ("form-control" :: classes)]
       ] @ divs)

let view_name state ids =
  let block, (title, (given, (second, (family, ())))) = ids in
  Node.div []
    (prepend_err_div state block
       [ input_str state "Titel" title
       ; input_str state "Vorname" given
       ; input_str state "Weitere Vornamen" second
       ; input_str state "Nachname" family
       ]
    )

let view_company state ids =
  let block, (name, (address, ())) = ids in
  Node.div []
    (prepend_err_div state block
       [ input_str state "Firma" name
       ; input_str state "Abteilung" address
       ]
    )

let view_address state ids =
  let block, (street, (number, (postal_code, (
      city, (country, (country_code, ())))))) = ids in
  Node.div []
    (prepend_err_div state block
       [ input_str state "Straße" street
       ; input_str state "Hausnummer" number
       ; input_str state "Postleitzahl" postal_code
       ; input_str state "Ort" city
       ; input_str state "Land" country
       ; input_str state "Ländercode" country_code
       ]
    )

let view_contact state ids =
  let block, (phone, (phone2, (mobile, (
      fax, (fax2, (mail, (mail2, (web, ())))))))) = ids in
  Node.div []
    (prepend_err_div state block
       [ input_str state "Telefon" phone
       ; input_str state "Telefon" phone2
       ; input_str state "Mobil" mobile
       ; input_str state "Fax" fax
       ; input_str state "Fax" fax2
       ; input_str state "Mail" mail
       ; input_str state "Mail" mail2
       ; input_str state "Internet" web
       ])

let view_period state ids =
  let from, till = ids in
  Node.div []
    [ input_str state "Von" from
    ; input_str state "Bis" till
    ]

let view_room state ids =
  let block, (room, (beds, (price_per_bed, (factor, (description, (period, ()
                                                      )))))) = ids in
  Node.div []
    (prepend_err_div state block
       [ input_str state "Zimmer" room
       ; input_str state "Betten" beds
       ; input_str state "Preis" price_per_bed
       ; input_str state "Faktor" factor
       ; input_str state "Beschreibung" description
       ; view_period state period
       ])

let view_guest state ids =
  let (block, (given, (second, (family, (born, ()))))) = ids in
  Node.div []
    ( prepend_err_div state block
        [ input_str state "Vorname" given
        ; input_str state "Weitere Vornamen" second
        ; input_str state "Nachname" family
        ; input_str state "Geburtstag" born
        ])

let view_booking state ids =
  let (block, (deposit_asked, (deposit_got, (no_tax, (note, (
      (guests,_), ((rooms,_), ()))))))) = ids in
  let guests = List.map guests ~f:(fun ids ->
      view_guest state ids)
  and rooms = List.map rooms ~f:(fun ids ->
      view_room state ids)
  in
  Node.div []
    ( prepend_err_div state block
        [ input_str state "Anzahlung gefordert" deposit_asked
        ; input_str state "Anzahlung erhalten" deposit_got
        ; input_bool state "Steuerfrei" no_tax
        ; input_str state "Notiz" note
        ] @ guests @ rooms )

let view_booking_list ~selected ~inject (l : Booking.t list) : Vdom.Node.t =
  let f i b =
    let s = Booking.summarize b in
    let f, t = match s.period with
      | None -> "n/a", "n/a"
      | Some p ->
        let open Period in Date.to_string (from p), Date.to_string (till p)
    in
    let e = Attr.[ on_click (fun _ -> inject (Action.SelectBooking i))
                 ; style (Css.create ~field:"cursor" ~value:"pointer") ]
    in
    let attr =
      if i = selected
      then Attr.class_ "table-active" :: e
      else e in
    Node.(
      tr attr
        [ td [] [text f]
        ; td [] [text t] ])
  in Node.(
      table [ Attr.classes ["table"; "table-sm"; "table-hover"] ]
        [ thead [] [ tr [] [ th [] [text "von"]
                           ; th [] [text "bis"] ] ]
        ; tbody [] ( List.mapi ~f l )
        ])

let view (model : Model.t Incr.t) ~inject ~save : Vdom.Node.t Incr.t =
  let open Vdom in
  let%map customer_state = model >>| Model.customer
  and booking_state = model >>| Model.booking
  and bookings = model >>| Model.bookings
  and selected = model >>| Model.selected
  in let
    (block_id,
     (name_block,
      (company_block,
       (address_block,
        (contact_block,
         (keyword_id,
          (note_id,
           ((),())))))))) =
    Form.State.field_ids customer_state customer_form
  in
  let save _evt =
    let c_opt, new_state = Form.State.read_value customer_state customer_form in
    match c_opt with
    | None -> inject (Action.UpdateCustomer new_state)
    | Some c -> save { c with bookings }
  and left = Node.div []
      [ input_str customer_state "Schlüsselwort" keyword_id
      ; view_name customer_state name_block
      ; view_company customer_state company_block
      ]
  and middle = view_address customer_state address_block
  and right = view_contact customer_state contact_block
  and booking_ids =
    Form.State.field_ids booking_state booking_form
  in
  Node.create "form" [] (
      Bs.rows
        [ [ Bs.button save "Speichern" ]
        ; prepend_err_div customer_state block_id []
        ; [ left; middle; right ]
        ; [ input_str ~input:Form.Input.textarea customer_state "Notiz" note_id ]
        ; [ view_booking_list ~inject bookings ~selected
          ; view_booking booking_state booking_ids]
        ])

let create
    ~(save: Customer.t -> Vdom.Event.t)
    ~(inject: Action.t -> Vdom.Event.t)
    (model:Model.t Incr.t) =
  let%map model = model
  and view = view ~inject ~save model in
  let apply_action = apply_action model in
  Component.create ~apply_action model view
