open Core_kernel
open Ghm
open Incr_dom
module Incr_map = Incr_map.Make (Incr)
module Form = Incr_dom_widgets.Form

module Model = struct
  (* TODO: make this Customer.with_id? *)
  type customer =
    { id : int
    ; data : Customer.t }
  [@@deriving compare]

  type view =
    | Overview
    | Customer
    | Letter
  [@@deriving compare]

  type t =
    { customers : customer Int.Map.t
    ; customer_table : Customer_table.Model.t
    ; customer_form : Customer_form.Model.t
    ; letter : Letter.Model.t
    ; view : view
    ; last_search : string
    ; search : Form.State.t }
  [@@deriving compare, fields]

  let cutoff t1 t2 = compare t1 t2 = 0
end

let search_form = Form.(create ~name:"keyword search" Description.string)

let init () : Model.t =
  { Model.customers = Int.Map.empty
  ; customer_table = Customer_table.Model.create ()
  ; customer_form = Customer_form.Model.create ()
  ; letter = Letter.Model.create ()
  ; view = Model.Overview
  ; last_search = ""
  ; search = Form.State.create ~init:"" search_form }
;;

module Action = struct
  type t =
    | NavChange of Nav.t option sexp_opaque
    | Search
    | ResetSearch
    | CustomerTable of Customer_table.Action.t
    | CustomerForm of Customer_form.Action.t
    | Letter of Letter.Action.t
    | GotCustomers of (int * Customer.t) list Or_error.t
  [@@deriving sexp_of, variants]
end

module State = struct
  type t = unit
end

let view_head inject last_search state =
  let open Vdom in
  let fld_id = Form.State.field_ids state search_form in
  Node.create
    "form"
    [Attr.on "submit" (fun _ -> inject Action.Search)]
    [ Bs.Grid.(
        frow
          ~c:["mb-4"; "mt-2"]
          [ col_auto
              [ div
                  [A.class_ "input-group"]
                  [ div
                      [A.class_ "input-group-prepend"]
                      [ Bs.button
                          ~i:(S "undo")
                          ~action:(fun _ -> inject Action.ResetSearch)
                          "Zurücksetzen" ]
                  ; Form.Input.text
                      state
                      fld_id
                      [ Attr.class_ "form-control"
                      ; Attr.placeholder "Schlüsselwort"
                      ; Attr.value last_search ]
                  ; div [A.class_ "input-group-append"] [Bs.button_submit "Suchen"] ] ]
          ; col
              [ frow
                  ~c:["justify-content-end"]
                  [Bs.button' ~href:Nav.(href_of (Customer New)) "Neuer Kunde"] ] ]) ]
;;

let get_customers ~schedule_action ?filter () =
  Request.XHR.send'
    Remote.Customers.(get ~limit:250 ?filter ())
    ~handler:(Fn.compose schedule_action Action.gotcustomers)
;;

let create model ~old_model ~inject =
  let open Incr.Let_syntax in
  let customers = model >>| Model.customers in
  let table =
    let model = model >>| Model.customer_table
    and old_model = old_model >>| Model.customer_table
    and inject = Fn.compose inject Action.customertable
    and rows =
      Incr_map.mapi customers ~f:(fun ~key:_ ~data ->
          Customer_table.Model.Row.of_customer ~id:data.id data.data )
    in
    Customer_table.create rows ~old_model ~inject ~model
  in
  let customer =
    let inject = Fn.compose inject Action.customerform
    and back_href = Nav.(href_of Overview)
    and form_model = model >>| Model.customer_form in
    Customer_form.create ~inject ~back_href form_model
  and letter =
    let inject = Fn.compose inject Action.letter
    and letter_model = model >>| Model.letter in
    Letter.create ~inject letter_model
  in
  let%map table = table
  and model = model
  and customer = customer
  and letter = letter
  and search_state = model >>| Model.search
  and last_search = model >>| Model.last_search in
  let apply_action (a : Action.t) _state ~schedule_action =
    match a with
    | GotCustomers (Error e) ->
      (* TODO: the gui should react to this *)
      Log.error e;
      model
    | GotCustomers (Ok l) ->
      let open Model in
      let customers =
        List.mapi l ~f:(fun i (id, data) -> i, {id; data})
        |> Int.Map.of_alist_or_error
        |> function
        | Error e ->
          Log.error e;
          Int.Map.empty
        | Ok m -> m
      in
      {model with customers}
    | CustomerTable a ->
      let schedule_action = Fn.compose schedule_action Action.customertable in
      let customer_table = Component.apply_action ~schedule_action table a () in
      {model with customer_table}
    | CustomerForm a ->
      let schedule_action = Fn.compose schedule_action Action.customerform in
      let customer_form = Component.apply_action ~schedule_action customer a () in
      {model with customer_form}
    | Letter a ->
      let schedule_action = Fn.compose schedule_action Action.letter in
      let letter = Component.apply_action ~schedule_action letter a () in
      {model with letter}
    | NavChange None ->
      Nav.(set Overview);
      model
    | NavChange (Some (Customer x)) ->
      schedule_action (Action.CustomerForm (Customer_form.Action.navchange x));
      {model with view = Customer}
    | NavChange (Some Overview) -> {model with view = Overview}
    | NavChange (Some Letter) -> {model with view = Letter}
    | Search ->
      let pattern_o, search = Form.State.read_value model.search search_form in
      (match pattern_o with
      | None -> model
      | Some s ->
        get_customers ~schedule_action ~filter:(Keyword s) ();
        {model with search; last_search = s})
    | ResetSearch ->
      get_customers ~schedule_action ();
      {model with last_search = ""; search = Form.State.create search_form}
  and view =
    let open Vdom in
    match model.view with
    | Overview ->
      Node.div
        [Attr.on "scroll" (fun _ -> Event.Viewport_changed)]
        [view_head inject last_search search_state; Component.view table]
    | Customer -> Component.view customer
    | Letter -> Component.view letter
  and update_visibility ~schedule_action : Model.t =
    let schedule_action = Fn.compose schedule_action Action.customertable in
    let customer_table = Component.update_visibility table ~schedule_action in
    {model with customer_table}
  and on_display _state ~schedule_action =
    let schedule_action = Fn.compose schedule_action Action.customertable in
    Component.on_display table ~schedule_action ()
  in
  Component.create ~update_visibility ~apply_action ~on_display model view
;;

let on_startup ~schedule_action _model =
  Nav.listen (Fn.compose schedule_action Action.navchange);
  schedule_action (Action.NavChange (Nav.get ()));
  get_customers ~schedule_action ();
  Async_kernel.Deferred.unit
;;
