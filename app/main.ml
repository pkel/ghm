open Core_kernel
open Incr_dom
module Incr_map = Incr_map.Make (Incr)
module State = State

module Model = struct
  type t =
    { agenda_view : Agenda_view.Model.t
    ; search_view : Search_view.Model.t
    ; customer_view : Customer_view.Model.t
    ; errors : Errors.Model.t
    ; nav : Nav.main
    }
  [@@deriving compare, fields]

  let cutoff t1 t2 = compare t1 t2 = 0
end

let init () : Model.t =
  { agenda_view = Agenda_view.Model.create ()
  ; search_view = Search_view.Model.create ()
  ; customer_view = Customer_view.Model.create ()
  ; errors = Errors.Model.empty
  ; nav = Nav.Overview
  }
;;

module Action = struct
  type t =
    | NavChange of Nav.main
    | Agenda_view of Agenda_view.Action.t
    | Search_view of Search_view.Action.t
    | Customer_view of Customer_view.Action.t
    | Errors of Errors.Action.t
  [@@deriving sexp_of, variants]
end

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
  let%map model = model
  and agenda =
    let inject = Fn.compose inject Action.agenda_view
    and model = model >>| Model.agenda_view in
    Agenda_view.create ~inject model
  and search =
    let inject = Fn.compose inject Action.search_view
    and model = model >>| Model.search_view
    and old_model = old_model >>| Model.search_view in
    Search_view.create ~old_model ~inject model
  and customer =
    let inject = Fn.compose inject Action.customer_view
    and model = model >>| Model.customer_view in
    Customer_view.create ~inject model
  and errors =
    let inject = Fn.compose inject Action.errors
    and model = model >>| Model.errors in
    Errors.create ~inject model
  in
  let apply_action (a : Action.t) (s : State.t) ~schedule_action =
    match a with
    | Agenda_view a ->
      let schedule_action = Fn.compose schedule_action Action.agenda_view in
      let agenda_view = Component.apply_action ~schedule_action agenda a s in
      { model with agenda_view }
    | Search_view a ->
      let schedule_action = Fn.compose schedule_action Action.search_view in
      let search_view = Component.apply_action ~schedule_action search a s in
      { model with search_view }
    | Customer_view a ->
      let schedule_action = Fn.compose schedule_action Action.customer_view in
      let customer_view = Component.apply_action ~schedule_action customer a s in
      { model with customer_view }
    | Errors a ->
      let schedule_action = Fn.compose schedule_action Action.errors in
      let errors = Component.apply_action ~schedule_action errors a s in
      { model with errors }
    | NavChange nav ->
      let () =
        match nav with
        | Customer x ->
          schedule_action (Action.customer_view (Customer_view.Action.navchange x))
        | Overview -> schedule_action (Action.agenda_view Agenda_view.Action.refresh)
        | Search -> schedule_action (Action.search_view Search_view.Action.refresh)
      in
      { model with nav }
  and view =
    let open Vdom in
    let attr, page, submenu =
      match model.nav with
      | Overview -> [], [ Component.view agenda ], []
      | Search -> [], [ Component.view search ], []
      | Customer _ -> [], [ Component.view customer ], Component.extra customer
    in
    let sidemenu =
      [ Menu.entry "Übersicht" (Menu.Href Nav.(href Overview)) (model.nav = Overview)
      ; Menu.entry "Suchen" (Menu.Href Nav.(href Search)) (model.nav = Search)
      ]
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
  and update_visibility ~schedule_action =
    let schedule_action = Fn.compose schedule_action Action.search_view in
    let search_view = Component.update_visibility search ~schedule_action in
    { model with search_view }
  and on_display state ~schedule_action =
    let schedule_action = Fn.compose schedule_action Action.search_view in
    Component.on_display search ~schedule_action state
  in
  Component.create ~update_visibility ~apply_action ~on_display model view
;;

let on_startup ~schedule_action _model =
  (* Wait for jwt token before doing anything else *)
  let open Async_kernel in
  Nav.listen ~handler:(Fn.compose schedule_action Action.navchange);
  Xhr.connect ()
  >>|
  let handle_error e = schedule_action (Action.Errors (Errors.Action.log e)) in
  function
  | None ->
    let open Browser in
    Window.alert window "Authentifizierung fehlgeschlagen (token)";
    Location.replace (Window.location window) "?action=logout";
    assert false
  | Some connection ->
    schedule_action (Action.NavChange (Nav.get ()));
    State.{ handle_error; connection }
;;
