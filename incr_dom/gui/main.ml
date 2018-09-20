open Core_kernel
open Ghm
open Async_kernel
open Incr_dom

module Model = struct

  type t =
    { customers : (Customer.t * Booking.t list) Int.Map.t
    ; table : Table.Model.t
    }
  [@@deriving compare, fields]

  let cutoff t1 t2 = compare t1 t2 = 0

  let init () =
    { customers = Storage.empty
    ; table = Table.Model.create
          ~scroll_margin:(Incr_dom_widgets.Table.Margin.uniform 5.)
          ~scroll_region:Window
          ~float_header:Edge
          ~float_first_col:Edge
          ~height_guess:43.
          ()
    }
end

let init ()  = Model.init ()

module Action = struct
  type t =
    | LoadDump of string
    | Table of Table.Action.t
  [@@deriving sexp_of]
end

module State = struct
  type t = unit
end

let render_row = fun ~row_id:_ ~row -> Row.view row

let create_table_component (model : Model.t Incr.t) ~old_model ~inject =
  let open Incr.Let_syntax in
  let columns = List.map ~f:Column.to_table_widget_column Row.Model.columns in
  let columns = Incr.const (List.mapi columns ~f:(fun i col -> i, col)) in
  let rows = model >>| Model.customers >>| Int.Map.map ~f:fst
  and table = model >>| Model.table
  and old_table = old_model >>| Model.table >>| Option.some
  in Table.create table
    ~old_model:old_table
    ~rows
    ~columns
    ~render_row
    ~inject:(fun a -> inject (Action.Table a))
    ~attrs:[]

let on_display table =
  let open Incr.Let_syntax in
  let%map table_on_display = table >>| Component.on_display in
  fun state ~schedule_action ->
    let schedule_table_action action = schedule_action (Action.Table action) in
    table_on_display state ~schedule_action:schedule_table_action

let update_visibility table (m : Model.t Incr.t) =
  let open Incr.Let_syntax in
  let%map m = m
  and table_update_visibility = table >>| Component.update_visibility in
  fun () ->
    let table = table_update_visibility () in
    { m with table }

let create model ~old_model ~inject =
  let open Incr.Let_syntax in
  let table = create_table_component ~old_model ~inject model in
  let%map apply_action =
    let%map model = model
    and table_apply_action = table >>| Component.apply_action in
    fun (a : Action.t) state ~schedule_action ->
      let schedule_table_action action =
        schedule_action (Action.Table action) in
      let apply_table_action action =
        table_apply_action action state ~schedule_action:schedule_table_action
      in
      match a with
      | LoadDump s ->
        { model with Model.customers = Storage.of_string s }
      | Table a ->
        { model with table = apply_table_action a }
  and view =
    let%map counter =
      let%map size = model >>| Model.customers >>| Storage.size in
      Vdom.Node.div [] [ Vdom.Node.text (Int.to_string size)
                       ; Vdom.Node.text " Kunden geladen."]
    and table = table >>| Component.view
    in
    Vdom.(Node.body [] [ counter
                       ; table ])
  and update_visibility = update_visibility table model
  and on_display = on_display table
  and model = model in
  Component.create ~update_visibility ~apply_action ~on_display model view

let on_startup ~schedule_action _model =
  Async_js.Http.get "data.sexp" >>| function
  | Ok s -> schedule_action (Action.LoadDump s)
  | _ -> ()
