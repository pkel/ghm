open Core_kernel
open Incr_dom
module Incr_map = Incr_map.Make (Incr)
module State = State

module Model = struct
  type t =
    { customers : Pg.Customers.return Int.Map.t option
    ; customer_table : Customer_table.Model.t
    ; customer_form : Customer_form.Model.t
    ; errors : Errors.Model.t
    ; nav : Nav.main
    ; last_search : string option
    ; search_input : string
    ; page : int
    }
  [@@deriving compare, fields]

  let cutoff t1 t2 = compare t1 t2 = 0
end

let init () : Model.t =
  (* TODO: make form components lazy ? *)
  { Model.customers = None
  ; customer_table = Customer_table.Model.create ()
  ; customer_form = Customer_form.Model.create ()
  ; errors = Errors.Model.empty
  ; nav = Nav.Overview
  ; last_search = None
  ; search_input = ""
  ; page = 0
  }
;;

module Action = struct
  type t =
    | NavChange of Nav.main
    | Search
    | Search_input of string
    | ResetSearch
    | GetMore
    | CustomerTable of Customer_table.Action.t
    | CustomerForm of Customer_form.Action.t
    | Errors of Errors.Action.t
    | GotCustomers of int (* page *) * Pg.Customers.return list sexp_opaque Or_error.t
  [@@deriving sexp_of, variants]
end

let view_head ~on_input ~inject ~init =
  let open Vdom in
  let open Incr.Let_syntax in
  let%map input = Incr_dom_widgets.Interactive.render ~on_input ~inject (
                      Bs.Form.input ~placeholder:"Schlüsselwort" ~init "Label"
                    )
  in
  Node.create
    "form"
    [ Attr.on "submit" (fun _ -> inject Action.Search) ]
    [ Bs.Grid.(
        frow
          ~c:[ "mb-4"; "mt-2" ]
          [ col_auto
              [ div
                  [ A.class_ "input-group" ]
                  [ div
                      [ A.class_ "input-group-prepend" ]
                      [ Bs.button
                          ~i:(S "undo")
                          ~action:(fun _ -> inject Action.ResetSearch)
                          "Zurücksetzen"
                      ]
                  ; input
                  ; div [ A.class_ "input-group-append" ] [ Bs.button_submit "Suchen" ]
                  ]
              ]
          ])
    ]
;;

let customer_page_size = 250

let get_customers ~conn ~schedule_action ?(page = 0) ?filter () =
  let offset = if page > 0 then Some ((page * customer_page_size) + 1) else None in
  Xhr.send'
    ~c:conn
    Pg.(
      read
        ~order:[ desc Customers.modified ]
        ?offset
        ~limit:customer_page_size
        ?filter
        Customers.t)
    ~handler:(fun r -> schedule_action (Action.GotCustomers (page, r)))
;;

let search_filter_of_input s =
  let open String in
  let s = strip s in
  let l, r = is_prefix ~prefix:"_" s, is_suffix ~suffix:"_" s in
  match
    strip
      ~drop:(function
        | '_' -> true
        | _ -> false)
      s
  with
  | "" -> None
  | s ->
    let s = (if l then "" else "%") ^ s ^ if r then "" else "%" in
    let open Pg.String in
    Some Pg.(ilike Customers.keyword s)
;;

let rec view_menu depth entries =
  let open Vdom in
  let open Menu in
  let padding =
    Attr.style Css_gen.(padding_left (`Rem (0.3 +. (1.2 *. float_of_int depth))))
  in
  let node e =
    (match e.action with
    | Href s -> Node.a [ padding; Attr.href s ]
    | On_click a -> Node.a [ padding; Attr.on_click (fun _ -> a ()) ])
      [ Node.text e.title ]
  in
  Node.ul
    []
    (List.map
       ~f:(fun entry ->
         let tl =
           match entry.children with
           | [] -> []
           | l -> [ view_menu (depth + 1) l ]
         and attr = if entry.active then [ Attr.class_ "active" ] else [] in
         Node.li attr (node entry :: tl))
       entries)
;;

let view_menu = view_menu 0

let create model ~old_model ~inject =
  let open Incr.Let_syntax in
  let customers = model >>| Model.customers >>| Option.value ~default:Int.Map.empty in
  let table =
    let model = model >>| Model.customer_table
    and old_model = old_model >>| Model.customer_table
    and inject = Fn.compose inject Action.customertable
    and rows =
      Incr_map.mapi customers ~f:(fun ~key:_ ~data ->
          Customer_table.Model.Row.of_customer ~id:data.id data)
    in
    Customer_table.create rows ~old_model ~inject ~model
  and customer =
    let inject = Fn.compose inject Action.customerform
    and form_model = model >>| Model.customer_form in
    Customer_form.create ~inject form_model
  in
  let errors =
    let inject = Fn.compose inject Action.errors
    and model = model >>| Model.errors in
    Errors.create ~inject model
  in
  let%map table = table
  and model = model
  and customer = customer
  and errors = errors in
  let apply_action (a : Action.t) (s : State.t) ~schedule_action =
    let conn = s.connection in
    let old_nav = model.nav in
    let model =
      match a with
      | GotCustomers (_, Error detail) ->
        s.handle_error { gist = "Verbindungsfehler (Kunden)"; detail };
        model
      | GotCustomers (page, Ok l) ->
        let customers =
          List.mapi l ~f:(fun i x -> i + (page * customer_page_size), x)
          |> Int.Map.of_alist_or_error
          |> function
          | Error detail ->
            s.handle_error { gist = "Laden von Kunden fehlgeschlagen"; detail };
            model.customers
          | Ok m ->
            if page > 0
            then (
              let old = Option.value ~default:Int.Map.empty model.customers in
              match Int.Map.append ~lower_part:old ~upper_part:m with
              | `Ok m -> Some m
              | `Overlapping_key_ranges ->
                s.handle_error
                  { gist = "Laden von Kunden fehlgeschlagen"
                  ; detail = Error.of_string "Overlapping key ranges"
                  };
                model.customers)
            else Some m
        in
        { model with customers; page }
      | CustomerTable a ->
        let schedule_action = Fn.compose schedule_action Action.customertable in
        let customer_table = Component.apply_action ~schedule_action table a () in
        { model with customer_table }
      | CustomerForm a ->
        let schedule_action = Fn.compose schedule_action Action.customerform in
        let customer_form = Component.apply_action ~schedule_action customer a s in
        { model with customer_form }
      | Errors a ->
        let schedule_action = Fn.compose schedule_action Action.errors in
        let errors = Component.apply_action ~schedule_action errors a s in
        { model with errors }
      | NavChange nav ->
        let () =
          match nav with
          | Customer x ->
            schedule_action (Action.CustomerForm (Customer_form.Action.navchange x))
          | Overview -> Nav.set Search
          | Search ->
            let filter = Option.bind ~f:search_filter_of_input model.last_search in
            get_customers ~conn ?filter ~schedule_action ()
        in
        { model with nav }
      | Search ->
          let filter = search_filter_of_input model.search_input in
          get_customers ~conn ~schedule_action ?filter ();
          { model with last_search = Some model.search_input }
      | Search_input search_input -> { model with search_input }
      | GetMore ->
        let page = model.page + 1
        and filter = Option.bind ~f:search_filter_of_input model.last_search in
        get_customers ~page ~conn ~schedule_action ?filter ();
        model
      | ResetSearch ->
        get_customers ~conn ~schedule_action ();
        { model with last_search = None; search = Form.State.create search_form }
    in
    let () = if old_nav <> model.nav then Nav.set model.nav in
    model
  and view =
    let open Vdom in
    let attr, page, submenu =
      match model.nav with
      | Overview -> [], [], []
      | Search ->
        ( [ Attr.on "scroll" (fun _ -> Event.Viewport_changed) ]
        , ([ view_head inject last_search search_state
           ; (if Option.is_some model.customers
             then Component.view table
             else Bs.Grid.loading_row)
           ]
          @
          match model.customers with
          | Some m when Int.Map.length m = (model.page + 1) * customer_page_size ->
            let open Bs.Grid in
            [ row
                ~c:[ "justify-content-center" ]
                [ col_auto
                    ~c:[ "mb-2" ]
                    [ Bs.button ~action:(fun _ -> inject Action.GetMore) "Mehr" ]
                ]
            ]
          | _ -> [])
        , [] )
      | Customer _ -> [], [ Component.view customer ], Component.extra customer
    in
    let sidemenu =
      [ Menu.entry "Suchen" (Menu.Href Nav.(href Overview)) (model.nav = Overview) ]
      @ submenu
      @ [ Menu.entry "Neuer Kunde" (Menu.Href Nav.(href (Customer (New, CData)))) false ]
      |> view_menu
    in
    Node.div
      (Attr.id "main" :: Attr.class_ "row" :: attr)
      [ Component.view errors
      ; Node.create
          "nav"
          [ Attr.id "sidebar"; Attr.class_ "col-auto" ]
          [ Node.h4 [] [ Node.text "Menü" ]
          ; Node.hr [ Attr.class_ "mb-0" ]
          ; sidemenu
          ; Node.hr [ Attr.class_ "mt-0" ]
          ]
      ; Node.div [ Attr.id "page"; Attr.classes [ "col"; "pr-0" ] ] page
      ]
  and update_visibility ~schedule_action : Model.t =
    let schedule_action = Fn.compose schedule_action Action.customertable in
    let customer_table = Component.update_visibility table ~schedule_action in
    { model with customer_table }
  and on_display _state ~schedule_action =
    let schedule_action = Fn.compose schedule_action Action.customertable in
    Component.on_display table ~schedule_action ()
  in
  Component.create ~update_visibility ~apply_action ~on_display model view
;;

let on_startup ~schedule_action _model =
  (* Wait for jwt token before doing anything else *)
  let open Async_kernel in
  Nav.listen ~handler:(Fn.compose schedule_action Action.navchange);
  schedule_action (Action.NavChange (Nav.get ()));
  Xhr.connect ()
  >>|
  let handle_error e = schedule_action (Action.Errors (Errors.Action.log e)) in
  function
  | None ->
    let open Browser in
    Window.alert window "Authentifizierung fehlgeschlagen (token)";
    Location.replace (Window.location window) "?action=logout";
    assert false
  | Some connection -> State.{ handle_error; connection }
;;
