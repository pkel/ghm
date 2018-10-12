open Core_kernel
open Ghm
open Incr_dom
module Incr_map = Incr_map.Make(Incr)

type navigation =
  | Overview
  | Customer of int
[@@deriving sexp_of, compare]

module Model = struct

  type t =
    { customers : Storage.t
    ; customer_table : Table.Model.t
    ; customer_form : Customer_form.Model.t
    ; nav : navigation
    }
  [@@deriving compare, fields]

  let cutoff t1 t2 = compare t1 t2 = 0
end

let nav_of_string s =
  let drop c = Caml.List.mem c ['/';'#'] in
  match String.split ~on:'/' (String.strip ~drop s) with
  | ["overview"] -> Ok Overview
  | ["customer"; s] -> begin
      match int_of_string_opt s with
      | Some i -> Ok (Customer i)
      | None -> Error "int expected after /customer/"
    end
  | _ -> Error "unknown path"

let string_of_nav = function
  | Overview -> "/overview"
  | Customer i -> sprintf "/customer/%d" i

let href_of_nav n = "#" ^ string_of_nav n

let load_customer i (model: Model.t) : Model.t =
  match Storage.load model.customers i with
  | None -> { model with customer_form = Customer_form.Model.empty () }
  | Some c -> { model with customer_form = Customer_form.Model.load c }

let hashchange (model: Model.t) : Model.t =
  match nav_of_string (Browser.Location.get_hash ()) with
  | Error _ -> Browser.Location.set_hash (string_of_nav Overview);
    { model with nav = Overview }
  | Ok (Customer i) -> load_customer i { model with nav = Customer i }
  | Ok nav -> { model with nav }

let init () : Model.t =
  { customers = Storage.empty
  ; customer_table = Table.Model.create ()
  ; customer_form = Customer_form.Model.empty ()
  ; nav = Overview
  } |> hashchange

module Action = struct
  type t =
    | GotChunk of int * Yojson.Safe.json sexp_opaque
    | Hashchange
    | Navigate of navigation
    | CustomerTable of Table.Action.t
    | CustomerForm of Customer_form.Action.t
    | CustomerSave of Customer.t
    | CustomerSaved of Request.response sexp_opaque
  [@@deriving sexp_of, variants]
end

module State = struct
  type t = unit
end

let get_chunk ~schedule_action i =
  let url =
    Printf.sprintf
      "/api/customers?order=customer_id.desc&limit=333&offset=%i"
      (333 * i)
  in let handler = function
    | Ok s -> schedule_action (Action.GotChunk (i, s))
    | Error _ -> Log.error ("get_chunk: " ^ url)
  in Request.send ~v:GET handler url

let create model ~old_model ~inject =
  let open Incr.Let_syntax in
  let customers = model >>| Model.customers in
  let table =
    let model = model >>| Model.customer_table
    and old_model = old_model >>| Model.customer_table
    and select i = inject (Action.navigate (Customer i))
    and inject = Fn.compose inject Action.customertable
    in
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
  let apply_action =
    fun (a : Action.t) _state ~schedule_action ->
      match a with
      | GotChunk (i, y) ->
        let chunk =
          match Storage.of_yojson y with
          | Ok s -> s
          | Error s -> Log.error ("GotChunk: " ^ s); Storage.empty
        in
        let customers = Storage.add_chunk customers ~chunk in
        (* chunk not readable or end of pagination *)
        if Storage.size chunk > 0 then
          get_chunk ~schedule_action (i + 1);
        (* reload the view with the fresh data *)
        hashchange { model with customers }
      | CustomerTable a ->
        let schedule_action = Fn.compose schedule_action Action.customertable in
        let customer_table =
          Component.apply_action ~schedule_action table a () in
        { model with customer_table }
      | CustomerForm a ->
        let schedule_action = Fn.compose schedule_action Action.customerform in
        let customer_form =
          Component.apply_action ~schedule_action customer a () in
        { model with customer_form }
      | CustomerSave c -> begin
          match model.nav with
          | Customer i ->
            let () =
              match Storage.load customers i with
              | None -> (* new customer --> post *)
                Request.send
                  ~v:POST
                  ~prefer:"return=representation"
                  ~body:(`Assoc ["data", (Customer.to_yojson c)])
                  (fun r-> schedule_action (Action.customersaved r))
                  "/api/customers"
              | Some _ -> (* existing customer --> patch *)
                Request.send
                  ~v:PATCH
                  ~prefer:"return=representation"
                  ~body:(`Assoc ["data", (Customer.to_yojson c)])
                  (fun r-> schedule_action (Action.customersaved r))
                  (Printf.sprintf "/api/customers?customer_id=eq.%i" i)
            in
            { model with customers = Storage.save customers ~key:i ~data:c }
          | _ -> Log.error "Invalid CustomerSaved"; model
        end
      | CustomerSaved r -> begin
          match r with
          | Error _ -> Log.error "Customer Save Request failed"; model
          | Ok json ->
            match Storage.db_entry_of_yojson json with
            | Error _ -> Log.error "CustomerSaved not parseable"; model
            | Ok e ->
              let key, data = e.customer_id, e.data in
              let customers = Storage.save model.customers ~key ~data in
              schedule_action (Action.Navigate (Customer key));
              { model with customers }
        end
      | Navigate nav ->
        (* Sideeffect: triggers Hashchange *)
        Browser.Location.set_hash (string_of_nav nav);
        model
      | Hashchange -> hashchange model
  and view =
    let open Vdom in
    let body attrs divs = Node.body attrs
        [Node.div [Attr.class_ "container-fluid"] divs]
    in
    match model.nav with
    | Overview ->
      body [ Attr.on "scroll" (fun _ -> Event.Viewport_changed) ]
        [Component.view table]
    | Customer _i ->
      body [] [Component.view customer]
  and update_visibility ~schedule_action : Model.t =
    let schedule_action = Fn.compose schedule_action Action.customertable in
    let customer_table = Component.update_visibility table ~schedule_action in
    { model with customer_table }
  and on_display _state ~schedule_action =
    let schedule_action = Fn.compose schedule_action Action.customertable in
    Component.on_display table ~schedule_action ()
  in
  Component.create ~update_visibility ~apply_action ~on_display model view

let on_startup ~schedule_action _model =
  Browser.(
    Window.add_event_listener window "hashchange"
      (fun _ -> schedule_action Action.Hashchange) false);
  get_chunk ~schedule_action 0;
  Async_kernel.Deferred.unit
