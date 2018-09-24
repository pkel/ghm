open Core_kernel
open Ghm
open Async_kernel
open Incr_dom
module Incr_map = Incr_map.Make(Incr)

type navigation =
  | Overview
  | Customer of int
[@@deriving sexp_of, compare]

let navigate nav =
  (* Sideffect: This triggers the Hashchange event/action, i.e directly after
     setting the fragment, it is parsed for setting [model.nav]. *)
  let open Js_of_ocaml.Url in
  let open Printf in
  let s = match nav with
    | Overview -> "/customers"
    | Customer i -> sprintf "/customers/%d" i
  in Current.set_fragment s

let nav_from_url () =
  let invalid () = navigate Overview; Overview in
  let open Js_of_ocaml in
  match String.split ~on:'/' (Url.Current.get_fragment ()) with
  | [""; "customers"] -> Overview
  | [""; "customers" ; i] -> begin match int_of_string_opt i with
      | Some i -> Customer i
      | None -> invalid () end
  | _ -> invalid ()

module Model = struct

  type t =
    { customers : Storage.t
    ; customer_table : Customer_table.Model.t
    ; nav : navigation
    }
  [@@deriving compare, fields]

  let cutoff t1 t2 = compare t1 t2 = 0

  let init () =
    { customers = Storage.empty
    ; customer_table = Customer_table.Model.create ()
    ; nav = nav_from_url ()
    }
end

let init ()  = Model.init ()

module Action = struct
  type t =
    | GotChunk of int * string
    | Navigate of navigation
    | Hashchange
    | CustomerTable of Customer_table.Action.t
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
    and select x = inject (Action.Navigate (Customer x))
    and inject = Fn.compose inject Action.customertable
    in
    Customer_table.create customers ~old_model ~inject ~select ~model
  in
  let%map table = table
  and model = model
  and customers = customers in
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
      | Navigate nav -> navigate nav; model (* Triggers Hashchange *)
      | Hashchange ->
        { model with nav = nav_from_url () }
  and view =
    let open Vdom in
    match model.nav with
    | Overview ->
      let table = Component.view table in
      Node.body
        [ Attr.on "scroll" (fun _ -> Event.Viewport_changed)
        ; Attr.style Css.(height Length.percent100) ]
        [ table ]
    | Customer i ->
      let msg = Printf.sprintf "Kunde %d" i in
      Node.body
        [ Attr.on_click (fun _-> inject (Navigate Overview))]
        [ Node.text msg ]
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
