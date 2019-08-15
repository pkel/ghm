open Core_kernel
open Ghm
open Incr_dom
module Incr_map = Incr_map.Make (Incr)
module Form = Incr_dom_widgets.Form
module State = State

module Model = struct
  (* TODO: make this Customer.with_id? *)
  type customer =
    { id : int
    ; data : Customer.t
    }
  [@@deriving compare]

  type t =
    { customers : customer Int.Map.t option
    ; customer_table : Customer_table.Model.t
    ; customer_form : Customer_form.Model.t
    ; errors : Errors.Model.t
    ; nav : Nav.t
    ; last_search : string option
    ; search : Form.State.t
    ; page : int
    }
  [@@deriving compare, fields]

  let cutoff t1 t2 = compare t1 t2 = 0
end

let search_form = Form.(create ~name:"keyword search" Description.string)

let init () : Model.t =
  { Model.customers = None
  ; customer_table = Customer_table.Model.create ()
  ; customer_form = Customer_form.Model.create ()
  ; errors = Errors.Model.empty
  ; nav = Nav.Overview
  ; last_search = None
  ; page = 0
  ; search = Form.State.create ~init:"" search_form
  }
;;

module Action = struct
  type t =
    | NavChange of Nav.t option sexp_opaque
    | Search
    | ResetSearch
    | GetMore
    | CustomerTable of Customer_table.Action.t
    | CustomerForm of Customer_form.Action.t
    | Errors of Errors.Action.t
    | GotCustomers of int (* page *) * (int * Customer.t) list Or_error.t
  [@@deriving sexp_of, variants]
end

let view_head inject last_search state =
  let open Vdom in
  let fld_id = Form.State.field_ids state search_form in
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
                  ; Form.Input.text
                      state
                      fld_id
                      ([ Attr.class_ "form-control"; Attr.placeholder "Schlüsselwort" ]
                      @
                      match last_search with
                      | None -> []
                      | Some s -> [ Attr.value s ])
                  ; div [ A.class_ "input-group-append" ] [ Bs.button_submit "Suchen" ]
                  ]
              ]
          ])
    ]
;;

let customer_page_size = 250

let get_customers ~conn ~schedule_action ?(page = 0) ?filter () =
  let offset = if page > 0 then Some ((page * customer_page_size) + 1) else None in
  Request.XHR.send'
    Remote.(Customers.get ?offset ~limit:customer_page_size ?filter () |> finalize conn)
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
    Some (Remote.Customers.Keyword s)
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
          Customer_table.Model.Row.of_customer ~id:data.id data.data)
    in
    Customer_table.create rows ~old_model ~inject ~model
  in
  let customer =
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
  and errors = errors
  and search_state = model >>| Model.search
  and last_search = model >>| Model.last_search in
  let apply_action (a : Action.t) (s : State.t) ~schedule_action =
    let conn = s.connection in
    match a with
    | GotCustomers (_, Error detail) ->
      s.handle_error { gist = "Verbindungsfehler (Kunden)"; detail };
      model
    | GotCustomers (page, Ok l) ->
      let customers =
        List.mapi l ~f:(fun i (id, data) ->
            i + (page * customer_page_size), Model.{ id; data })
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
    | NavChange None ->
      let nav = Nav.Overview in
      Nav.(set Overview);
      { model with nav }
    | NavChange (Some nav) ->
      let () =
        match nav with
        | Customer x ->
          schedule_action (Action.CustomerForm (Customer_form.Action.navchange x))
        | Overview ->
          let filter = Option.bind ~f:search_filter_of_input model.last_search in
          get_customers ~conn ?filter ~schedule_action ()
      in
      { model with nav }
    | Search ->
      let pattern_o, search = Form.State.read_value model.search search_form in
      (match pattern_o with
      | None -> model
      | Some s ->
        let filter = search_filter_of_input s in
        get_customers ~conn ~schedule_action ?filter ();
        { model with search; last_search = Some s })
    | GetMore ->
      let page = model.page + 1
      and filter = Option.bind ~f:search_filter_of_input model.last_search in
      get_customers ~page ~conn ~schedule_action ?filter ();
      model
    | ResetSearch ->
      get_customers ~conn ~schedule_action ();
      { model with last_search = None; search = Form.State.create search_form }
  and view =
    let open Vdom in
    let attr, page, submenu =
      match model.nav with
      | Overview ->
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
      [ Menu.entry "Suchen" (Menu.Href Nav.(href_of Overview)) (model.nav = Overview) ]
      @ submenu
      @ [ Menu.entry "Neuer Kunde" (Menu.Href Nav.(href_of (Customer New))) false ]
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
  Nav.listen (Fn.compose schedule_action Action.navchange);
  schedule_action (Action.NavChange (Nav.get ()));
  Remote.connect ()
  >>| function
  | None -> assert false (* TODO: logout/redirect *)
  | Some connection ->
    let handle_error e = schedule_action (Action.Errors (Errors.Action.log e)) in
    State.{ handle_error; connection }
;;
