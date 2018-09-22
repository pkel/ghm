open Core_kernel
open Ghm
open Async_kernel
open Incr_dom
module Incr_map = Incr_map.Make(Incr)

module Model = struct

  type t =
    { customers : Customer.t Int.Map.t
    ; table : Table.Model.t
    ; pattern : string
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
    ; pattern = ""
    }
end

let init ()  = Model.init ()

module Action = struct
  type t =
    | GotChunk of int * string
    | Table of Table.Action.t
    | Pattern of string
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

let create_table customers pattern model ~old_model ~inject =
  let open Incr.Let_syntax in
  let%bind pattern = pattern in
  let columns = List.map ~f:Column.to_table_widget_column Row.Model.columns in
  let columns = Incr.const (List.mapi columns ~f:(fun i col -> i, col)) in
  let rows =
    Incr.Map.filter_mapi customers ~f:(fun ~key:_ ~data ->
        let row = Row.Model.of_customer data in
        Option.some_if (Row.Model.matches_pattern row pattern) row)
  and render_row = fun ~row_id:_ ~row -> Row.view row
  in Table.create model
    ~old_model
    ~rows
    ~columns
    ~render_row
    ~inject:(fun a -> inject (Action.Table a))
    ~attrs:[]

let create model ~old_model ~inject =
  let open Incr.Let_syntax in
  let customers = model >>| Model.customers
  and pattern = model >>| Model.pattern in
  let table =
    let model = model >>| Model.table
    and old_model = old_model >>| Model.table >>| Option.some in
    create_table customers pattern ~old_model ~inject model
  in
  let%map table = table
  and model = model
  and customers = customers
  and size = customers >>| Storage.size in
  let apply_action =
    fun (a : Action.t) _state ~schedule_action ->
      match a with
      | GotChunk (i, s) ->
        let chunk = Storage.of_string s in
        let customers = Storage.add_chunk customers ~chunk in
        don't_wait_for (get_chunk ~schedule_action (i + 1));
        { model with customers }
      | Table a ->
        let schedule_action = Fn.compose schedule_action Action.table in
        let table = Component.apply_action ~schedule_action table a () in
        { model with table }
      | Pattern pattern -> { model with pattern }
  and view =
    let open Vdom in
    let search =
      Node.div
        [ Attr.id "search-container" ]
        [ Node.input
            [ Attr.id "search-input"
            ; Attr.create "placeholder" "Suche"
            ; Attr.create "type" "text"
            ; Attr.on_input (fun _ev text -> inject (Pattern text))
            ]
            []
        ]
    in
    let counter =
      let s = Printf.sprintf "%d Kunden geladen." size in
      Node.div [] [ Node.text s ]
    and table = Component.view table in
    Node.body
      [ Attr.on "scroll" (fun _ -> Event.Viewport_changed)
      ; Attr.style Css.(height Length.percent100) ]
      [ counter
      ; search
      ; table ]
    and update_visibility ~schedule_action : Model.t =
      let schedule_action = Fn.compose schedule_action Action.table in
      let table = Component.update_visibility table ~schedule_action in
      { model with table }
    and on_display _state ~schedule_action =
      let schedule_action = Fn.compose schedule_action Action.table in
      Component.on_display table ~schedule_action ()
    in
    Component.create ~update_visibility ~apply_action ~on_display model view

let on_startup ~schedule_action _model =
  get_chunk ~schedule_action 0
