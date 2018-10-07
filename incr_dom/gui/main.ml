open Core_kernel
open Ghm
open Async_kernel
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

let rec navigate nav (model : Model.t) =
  (* Sideffect: This triggers the Hashchange event/action, i.e directly after
     setting the fragment, it is parsed for setting [model.nav]. *)
  let open Printf in
  let set = Js_of_ocaml.Url.Current.set_fragment in
  let m = match nav with
  | Overview -> set "/customers"; model
  | Customer i -> begin
    match Storage.load model.customers i with
    | None -> navigate Overview model
    | Some c ->
      set (sprintf "/customers/%d" i);
      { model with customer_form = Customer_form.Model.load c; }
  end
  in { m with nav }

(* These two should be cleaned up *)

let nav_from_url model =
  let open Js_of_ocaml in
  let invalid () = navigate Overview model in
  match String.split ~on:'/' (Url.Current.get_fragment ()) with
  | [""; "customers"] -> navigate Overview model
  | [""; "customers" ; i] -> begin match int_of_string_opt i with
      | Some i -> navigate (Customer i) model
      | None -> invalid () end
  | _ -> invalid ()

let init () : Model.t =
  { customers = Storage.empty
  ; customer_table = Table.Model.create ()
  ; customer_form = Customer_form.Model.empty ()
  ; nav = Overview
  } |> nav_from_url

module Action = struct
  type t =
    | GotChunk of int * string
    | Navigate of navigation
    | Hashchange
    | CustomerTable of Table.Action.t
    | CustomerForm of Customer_form.Action.t
    | CustomerSave of Customer.t
  [@@deriving sexp_of, variants]
end

module State = struct
  type t = unit
end

let get_chunk ~schedule_action i =
  let url= Printf.sprintf "../data/chunks/chunk-%d.sexp" i in
  Async_js.Http.get url >>| function
  | Ok s -> schedule_action (Action.GotChunk (i,s))
  | _ -> ()

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
    and form_model = model >>| Model.customer_form in
    Customer_form.create ~inject ~save form_model
  in
  let%map table = table
  and model = model
  and customers = customers
  and customer = customer in
  let apply_action =
    fun (a : Action.t) _state ~schedule_action ->
      match a with
      | GotChunk (i, s) ->
        let chunk = Storage.of_string s in
        let customers = Storage.add_chunk customers ~chunk in
        don't_wait_for (get_chunk ~schedule_action (i + 1));
        { model with customers }
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
            { model with customers = Storage.save customers ~key:i ~data:c }
          | _ -> Log.error "Invalid CustomerSaved"; model
        end
      | Navigate nav -> navigate nav model
      | Hashchange -> nav_from_url model
  and view =
    let open Vdom in
    let body attrs divs = Node.body attrs
        [Node.div [Attr.class_ "container-fluid"] divs]
    in
    match model.nav with
    | Overview ->
      body [ Attr.on "scroll" (fun _ -> Event.Viewport_changed) ]
        [ Bs.row [Component.view table] ]
    | Customer _i ->
      body []
        [ Bs.row [Bs.button (fun _-> inject (Navigate Overview)) "Zurück"]
        ; Bs.row [Component.view customer] ]
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
  let _ =
    let open Js_of_ocaml.Dom_html in
    let open Js_of_ocaml.Js in
    let handler = handler (fun _ -> schedule_action Action.Hashchange; _true) in
    addEventListener window Event.hashchange handler _false
  in
  get_chunk ~schedule_action 0
