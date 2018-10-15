open Core_kernel
open Ghm
open Incr_dom
module Incr_map = Incr_map.Make (Incr)

type navigation = Overview | Customer of int [@@deriving sexp_of, compare]

module Model = struct
  type t =
    { customers: Storage.t
    ; customer_table: Table.Model.t
    ; customer_form: Customer_form.Model.t
    ; nav: navigation }
  [@@deriving compare, fields]

  let cutoff t1 t2 = compare t1 t2 = 0
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
  | Some c -> {model with customer_form= Customer_form.Model.load c}

let hashchange (model : Model.t) : Model.t =
  match nav_of_string (Browser.Location.get_hash ()) with
  | Error _ ->
      Browser.Location.set_hash (string_of_nav Overview) ;
      {model with nav= Overview}
  | Ok (Customer i) -> load_customer i {model with nav= Customer i}
  | Ok nav -> {model with nav}

let init () : Model.t =
  { customers= Storage.empty
  ; customer_table= Table.Model.create ()
  ; customer_form= Customer_form.Model.empty ()
  ; nav= Overview }
  |> hashchange

module Action = struct
  type t =
    | Hashchange
    | Navigate of navigation
    | CustomerTable of Table.Action.t
    | CustomerForm of Customer_form.Action.t
    | CustomerSave of Customer.t
    | ResponseCustomerSaved of (int * Customer.t) Or_error.t
    | GotCustomers of Customer.t Int.Map.t
  [@@deriving sexp_of, variants]
end

module State = struct
  type t = unit
end

let create model ~old_model ~inject =
  let open Incr.Let_syntax in
  let customers = model >>| Model.customers in
  let table =
    let model = model >>| Model.customer_table
    and old_model = old_model >>| Model.customer_table
    and select i = inject (Action.navigate (Customer i))
    and inject = Fn.compose inject Action.customertable in
    Table.create customers ~old_model ~inject ~select ~model
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
  and customer = customer in
  let apply_action (a : Action.t) _state ~schedule_action =
    match a with
    | GotCustomers customers ->
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
            | None -> Request.send ~body:c ~handler Remote.Customer.post
            | Some _ ->
                let rq =
                  Request.map_resp (Remote.Customer.patch id) ~f:(fun c ->
                      (id, c) )
                in
                Request.send ~body:c ~handler rq
          in
          {model with customers= Storage.save customers ~key:id ~data:c}
      | _ ->
          Log.error_str "Invalid CustomerSave" ;
          model )
    | ResponseCustomerSaved r -> (
      match r with
      | Error e -> Log.error e ; model
      | Ok (key, data) ->
          let customers = Storage.save model.customers ~key ~data in
          let customer_form = Customer_form.Model.load data in
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
  and view =
    let open Vdom in
    let body attrs divs =
      Node.body attrs [Node.div [Attr.class_ "container-fluid"] divs]
    in
    match model.nav with
    | Overview ->
        body
          [Attr.on "scroll" (fun _ -> Event.Viewport_changed)]
          [Component.view table]
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
  Request.send'
    Remote.(Customers.get ~sort:(Desc Customers.Modified) ~limit:250 ())
    ~handler ;
  Async_kernel.Deferred.unit
