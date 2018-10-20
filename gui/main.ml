open Core_kernel
open Ghm
open Incr_dom
module Incr_map = Incr_map.Make (Incr)
module Form = Incr_dom_widgets.Form

type navigation = Overview | Customer of int [@@deriving sexp_of, compare]

module Model = struct
  type customer = {row: int; data: Customer.t} [@@deriving compare]

  type t =
    { customers: customer Int.Map.t
    ; customer_table: Customer_table.Model.t
    ; customer_form: Customer_form.Model.t
    ; nav: navigation
    ; search: Form.State.t }
  [@@deriving compare, fields]

  let cutoff t1 t2 = compare t1 t2 = 0

  let min_row_id =
    Int.Map.fold ~init:Int.max_value ~f:(fun ~key:_ ~data a -> min data.row a)
end

let nav_of_string s =
  let drop c = Caml.List.mem c ['/'; '#'] in
  match String.split ~on:'/' (String.strip ~drop s) with
  | ["overview"] -> Ok Overview
  | ["customer"; s] -> (
    match int_of_string_opt s with
    | Some i -> Ok (Customer i)
    | None -> Error "int expected after /customer/" )
  | _ -> Error "unknown path"

let string_of_nav = function
  | Overview -> "/overview"
  | Customer i -> sprintf "/customer/%d" i

let href_of_nav n = "#" ^ string_of_nav n

let load_customer i (model : Model.t) : Model.t =
  match Storage.load model.customers i with
  | None -> {model with customer_form= Customer_form.Model.empty ()}
  | Some c -> {model with customer_form= Customer_form.Model.load c.data}

let hashchange (model : Model.t) : Model.t =
  match nav_of_string (Browser.Location.get_hash ()) with
  | Error _ ->
      Browser.Location.set_hash (string_of_nav Overview) ;
      {model with nav= Overview}
  | Ok (Customer i) -> load_customer i {model with nav= Customer i}
  | Ok nav -> {model with nav}

let search_form = Form.(create ~name:"keyword search" Description.string)

let init () : Model.t =
  { customers= Storage.empty
  ; customer_table= Customer_table.Model.create ()
  ; customer_form= Customer_form.Model.empty ()
  ; nav= Overview
  ; search= Form.State.create ~init:"" search_form }
  |> hashchange

module Action = struct
  type t =
    | Hashchange
    | Navigate of navigation
    | Search
    | CustomerTable of Customer_table.Action.t
    | CustomerForm of Customer_form.Action.t
    | CustomerSave of Customer.t
    | ResponseCustomerSaved of (int * Customer.t) Or_error.t
    | GotCustomers of (int * Customer.t) list
  [@@deriving sexp_of, variants]
end

module State = struct
  type t = unit
end

let view_search inject state =
  let open Vdom in
  let fld_id = Form.State.field_ids state search_form in
  let col = Node.div [Attr.class_ "col-auto"] in
  Node.create "form"
    [Attr.on "submit" (fun _ -> inject Action.Search)]
    [ Node.div
        [Attr.classes ["form-row"]]
        [ col
            [ Form.Input.text state fld_id
                [Attr.class_ "form-control"; Attr.placeholder "Schlüsselwort"]
            ]
        ; col [Bs.submit "Suchen"] ] ]

let create model ~old_model ~inject =
  let open Incr.Let_syntax in
  let customers = model >>| Model.customers in
  let table =
    let model = model >>| Model.customer_table
    and old_model = old_model >>| Model.customer_table
    and select i = inject (Action.navigate (Customer i))
    and inject = Fn.compose inject Action.customertable
    and rows =
      Incr_map.unordered_fold customers ~init:Int.Map.empty
        ~add:(fun ~key:id ~data acc ->
          let key = data.row in
          let data = Customer_table.Model.Row.of_customer ~id data.data in
          Int.Map.set ~key ~data acc )
        ~remove:(fun ~key:_ ~data acc -> Int.Map.remove acc data.row)
    in
    Customer_table.create rows ~old_model ~inject ~select ~model
  in
  let customer =
    let inject = Fn.compose inject Action.customerform
    and save = Fn.compose inject Action.customersave
    and back_href = href_of_nav Overview
    and form_model = model >>| Model.customer_form in
    Customer_form.create ~inject ~save ~back_href form_model
  in
  let%map table = table
  and model = model
  and customers = customers
  and customer = customer
  and search_state = model >>| Model.search in
  let apply_action (a : Action.t) _state ~schedule_action =
    match a with
    | GotCustomers l ->
        let open Model in
        let customers =
          List.mapi l ~f:(fun i (id, c) -> (id, {row= i; data= c}))
          |> Int.Map.of_alist_or_error
          |> function Error e -> Log.error e ; Int.Map.empty | Ok m -> m
        in
        (* why hashchange? *)
        hashchange {model with customers}
    | CustomerTable a ->
        let schedule_action =
          Fn.compose schedule_action Action.customertable
        in
        let customer_table =
          Component.apply_action ~schedule_action table a ()
        in
        {model with customer_table}
    | CustomerForm a ->
        let schedule_action = Fn.compose schedule_action Action.customerform in
        let customer_form =
          Component.apply_action ~schedule_action customer a ()
        in
        {model with customer_form}
    | CustomerSave c -> (
      match model.nav with
      | Customer id ->
          let () =
            let handler x = schedule_action (Action.ResponseCustomerSaved x) in
            match Storage.load customers id with
            | None -> Request.XHR.send ~body:c ~handler Remote.Customer.post
            | Some _ ->
                let rq =
                  Request.map_resp (Remote.Customer.patch id) ~f:(fun c ->
                      (id, c) )
                in
                Request.XHR.send ~body:c ~handler rq
          in
          model
      | _ ->
          Log.error_str "Invalid CustomerSave" ;
          model )
    | ResponseCustomerSaved r -> (
      match r with
      | Error e -> Log.error e ; model
      | Ok (key, data) ->
          let open Model in
          let data =
            match Int.Map.find model.customers key with
            | Some {row; _} -> {row; data}
            | None -> {row= min_row_id model.customers - 1; data}
          in
          let customers = Storage.save model.customers ~key ~data in
          let customer_form = Customer_form.Model.load data.data in
          let () =
            match model.nav with
            | Customer i when i <> key ->
                schedule_action (Action.Navigate (Customer key))
            | _ -> ()
          in
          {model with customers; customer_form} )
    | Navigate nav ->
        (* Sideeffect: triggers Hashchange *)
        Browser.Location.set_hash (string_of_nav nav) ;
        model
    | Hashchange -> hashchange model
    | Search ->
        let pattern_o, search =
          Form.State.read_value model.search search_form
        in
        let () =
          match pattern_o with
          | None -> ()
          | Some s ->
              Request.XHR.send'
                Remote.Customers.(
                  get ~limit:250 ~sort:(Desc Modified) ~filter:(Keyword s) ())
                ~handler:(function
                  | Error e -> Log.error e
                  | Ok l -> schedule_action (Action.GotCustomers l))
        in
        {model with search}
  and view =
    let open Vdom in
    let body attrs divs =
      Node.body attrs [Node.div [Attr.class_ "container-fluid"] divs]
    in
    match model.nav with
    | Overview ->
        body
          [Attr.on "scroll" (fun _ -> Event.Viewport_changed)]
          (Bs.rows [[view_search inject search_state]; [Component.view table]])
    | Customer _i -> body [] [Component.view customer]
  and update_visibility ~schedule_action : Model.t =
    let schedule_action = Fn.compose schedule_action Action.customertable in
    let customer_table = Component.update_visibility table ~schedule_action in
    {model with customer_table}
  and on_display _state ~schedule_action =
    let schedule_action = Fn.compose schedule_action Action.customertable in
    Component.on_display table ~schedule_action ()
  in
  Component.create ~update_visibility ~apply_action ~on_display model view

let on_startup ~schedule_action _model =
  Browser.(
    Window.add_event_listener window "hashchange"
      (fun _ -> schedule_action Action.Hashchange)
      false) ;
  let handler = function
    | Ok page -> schedule_action (Action.GotCustomers page)
    | Error e -> Log.error e
  in
  Request.XHR.send'
    Remote.(Customers.get ~sort:(Desc Customers.Modified) ~limit:250 ())
    ~handler ;
  Async_kernel.Deferred.unit
