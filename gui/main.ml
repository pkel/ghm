open Core_kernel
open Ghm
open Incr_dom
module Incr_map = Incr_map.Make (Incr)
module Form = Incr_dom_widgets.Form

module Location : sig
  type t = string list [@@deriving sexp_of]

  val href_of : t -> string
  val listen : (t -> unit) -> unit
  val set : t -> unit
  val get : unit -> t
end = struct
  type t = string list [@@deriving sexp_of]

  let of_string s =
    let drop c = Caml.List.mem c ['/'; '#'] in
    String.split ~on:'/' (String.strip ~drop s)
  ;;

  let string_of p = "/" ^ String.concat ~sep:"/" p
  let href_of p = "#" ^ string_of p
  let set t = Browser.Location.set_hash (string_of t)
  let get () = of_string (Browser.Location.get_hash ())

  let listen on_change =
    let open Browser in
    Window.add_event_listener window "hashchange" (fun _ -> on_change (get ())) false
  ;;
end

module Navigation = struct
  module Customer = Customer_form.Navigation

  type t =
    | Overview
    | Customer of Customer.t
  [@@deriving sexp_of, compare, variants]

  let of_path = function
    | ["overview"] -> Some Overview
    | "customer" :: tl -> Option.map ~f:customer (Customer.of_path tl)
    | _ -> None
  ;;

  let path_of = function
    | Overview -> ["overview"]
    | Customer n -> "customer" :: Customer.to_path n
  ;;
end

module Model = struct
  (* TODO: make this Customer.with_id? *)
  type customer =
    { id : int
    ; data : Customer.t }
  [@@deriving compare]

  type view =
    | Overview
    | Customer
  [@@deriving compare]

  type t =
    { customers : customer Int.Map.t
    ; customer_table : Customer_table.Model.t
    ; customer_form : Customer_form.Model.t
    ; view : view
    ; search : Form.State.t }
  [@@deriving compare, fields]

  let cutoff t1 t2 = compare t1 t2 = 0
end

let search_form = Form.(create ~name:"keyword search" Description.string)

let init () : Model.t =
  { Model.customers = Int.Map.empty
  ; customer_table = Customer_table.Model.create ()
  ; customer_form = Customer_form.Model.create ()
  ; view = Model.Overview
  ; search = Form.State.create ~init:"" search_form }
;;

module Action = struct
  type t =
    | Navigate of Navigation.t
    | Location of Location.t
    | Search
    | CustomerTable of Customer_table.Action.t
    | CustomerForm of Customer_form.Action.t
    | GotCustomers of (int * Customer.t) list
  [@@deriving sexp_of, variants]
end

module State = struct
  type t = unit
end

let view_head inject state =
  let open Vdom in
  let fld_id = Form.State.field_ids state search_form in
  let newc = Location.href_of Navigation.(path_of (Customer Customer.new_)) in
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
                      [Bs.button' ~i:(S "undo") ~href:"TODO" "Zurücksetzen"]
                  ; Form.Input.text
                      state
                      fld_id
                      [Attr.class_ "form-control"; Attr.placeholder "Schlüsselwort"]
                  ; div [A.class_ "input-group-append"] [Bs.submit "Suchen"] ] ]
          ; col [frow ~c:["justify-content-end"] [Bs.button' ~href:newc "Neuer Kunde"]]
          ]) ]
;;

let create model ~old_model ~inject =
  let open Incr.Let_syntax in
  let customers = model >>| Model.customers in
  let table =
    let model = model >>| Model.customer_table
    and old_model = old_model >>| Model.customer_table
    and select i = inject (Action.navigate Navigation.(Customer (Customer.id i)))
    and inject = Fn.compose inject Action.customertable
    and rows =
      Incr_map.mapi customers ~f:(fun ~key:_ ~data ->
          Customer_table.Model.Row.of_customer ~id:data.id data.data )
    in
    Customer_table.create rows ~old_model ~inject ~select ~model
  in
  let customer =
    let inject = Fn.compose inject Action.customerform
    and back_href = Location.href_of (Navigation.path_of Overview)
    and form_model = model >>| Model.customer_form in
    Customer_form.create ~inject ~back_href form_model
  in
  let%map table = table
  and model = model
  and customer = customer
  and search_state = model >>| Model.search in
  let apply_action (a : Action.t) _state ~schedule_action =
    match a with
    | GotCustomers l ->
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
    | Navigate nav ->
      (* Sideeffect: triggers Location Action *)
      Location.set (Navigation.path_of nav);
      model
    | Location path ->
      (match Navigation.of_path path with
      | None ->
        Location.set Navigation.(path_of Overview);
        model
      | Some (Customer x) ->
        schedule_action (Action.CustomerForm (Customer_form.Action.navigate x));
        {model with view = Customer}
      | Some Overview -> {model with view = Overview})
    | Search ->
      let pattern_o, search = Form.State.read_value model.search search_form in
      let () =
        match pattern_o with
        | None -> ()
        | Some s ->
          Request.XHR.send'
            Remote.Customers.(get ~limit:250 ~filter:(Keyword s) ())
            ~handler:(function
              | Error e -> Log.error e | Ok l -> schedule_action (Action.GotCustomers l))
      in
      {model with search}
  and view =
    let open Vdom in
    let body attrs divs =
      Node.body attrs [Node.div [Attr.class_ "container-fluid"] divs]
    in
    match model.view with
    | Overview ->
      body
        [Attr.on "scroll" (fun _ -> Event.Viewport_changed)]
        [view_head inject search_state; Component.view table]
    | Customer -> body [] [Component.view customer]
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
  Location.listen (Fn.compose schedule_action Action.location);
  schedule_action (Action.Location (Location.get ()));
  (* TODO: it might be cleaner to change the Action type to Or_error *)
  let handler = function
    | Ok page -> schedule_action (Action.GotCustomers page)
    | Error e -> Log.error e
  in
  Request.XHR.send' Remote.(Customers.get ~limit:250 ()) ~handler;
  Async_kernel.Deferred.unit
;;
