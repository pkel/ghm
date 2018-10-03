open Ghm
open Core_kernel
open Incr_dom
open Vdom
open Incr.Let_syntax

(* TODO: Get rid of unnecessary code form ts_gui example *)

module Table = struct
  open Incr_dom_widgets
  module RowId = struct
    include Core_kernel.Int
    let compare a b = compare b a
  end
  include Table.Make (RowId) (Core_kernel.Int) (Table.Default_sort_spec)
end

module Column = struct
  open Core_kernel
  open Incr_dom
  module Sort_key = Incr_dom_widgets.Table.Default_sort_spec.Sort_key

  module type CONTENTS = sig
    type t
    val to_string: t -> string
  end

  module type T = sig
    type row

    module Contents : CONTENTS

    val name : string
    val group : string option
    val get : row -> Contents.t
    val sort_by : Contents.t -> Sort_key.t
  end

  let create
      (type row contents)
      ~name
      ?group
      ?sort_by
      (module Contents : CONTENTS with type t = contents)
      ~get
    =
    let sort_by =
      match sort_by with
      | Some f -> f
      | None -> fun x -> Sort_key.String (Contents.to_string x)
    in
    ( module struct
      type nonrec row = row

      module Contents = Contents

      let name = name
      let group = group
      let get = get
      let sort_by = sort_by
    end : T with type row = row )

  let of_field
      (type contents)
      field
      ?name
      ?group
      ?sort_by
      (module Contents : CONTENTS with type t = contents)
    =
    let name = Option.value ~default:(Field.name field) name in
    create
      ~name
      ?group
      ?sort_by
      (module Contents)
      ~get:(Field.get field)

  let name (type row) (module T : T with type row = row) = T.name
  let group (type row) (module T : T with type row = row) = T.group

  let get (type row) (module T : T with type row = row) row =
    T.Contents.to_string (T.get row)

  let sort_by (type row) (module T : T with type row = row) row = T.sort_by (T.get row)

  let to_table_widget_column t =
    let name = name t in
    let group = group t in
    let sort_by _row_id row = sort_by t row in
    Table.Column.create ~header:(Vdom.Node.text name) ~sort_by ?group ()
end

module Row = struct
  module Rn_spec = Incr_dom_widgets.Row_node_spec
  module Sort_key = Incr_dom_widgets.Table.Default_sort_spec.Sort_key


  module IntOpt = struct
    type t = int option

    let to_string t =
      Option.map ~f:string_of_int t
      |> Option.value ~default:""

    let sort_key x = Sort_key.Integer
        Option.(map ~f:Int63.of_int x |> value ~default:Int63.zero)
  end

  module DateOpt = struct
    type t = Date.t option

    let string_of_date d =
      let open Date in
      Printf.sprintf "%02d.%02d.%04d" (day d) (month d |> Month.to_int) (year d)

    let to_string t =
      Option.map ~f:string_of_date t
      |> Option.value ~default:""

    let sort_key x = Sort_key.String (
        Option.map ~f:Date.to_string_iso8601_basic x
        |> Option.value ~default:"")
  end

  module Model = struct
    type t =
      { keyword: string
      ; given: string
      ; family: string
      ; company: string
      (* last booking *)
      ; from: Date.t option
      ; till: Date.t option
      ; rooms: int option
      ; beds: int option
      ; guests: int option
      } [@@deriving compare, fields]

    let columns =
      let append f list field = f field :: list in
      let add ?group ?sort_by m name =
        append (fun field ->
            Column.of_field field m ~name ?group ?sort_by)
      in
      let dsc = "Stammdaten"
      and visit = "Letzter Aufenthalt" in
      let lex_s x = Sort_key.String x in
      let dat_s = DateOpt.sort_key in
      let int_s = IntOpt.sort_key in
      Fields.fold
        ~init:[]
        ~keyword:(add (module String) ~sort_by:lex_s "Schlüssel")
        ~given:(add (module String) ~sort_by:lex_s "Vorname" ~group:dsc)
        ~family:(add (module String) ~sort_by:lex_s "Nachname" ~group:dsc)
        ~company:(add (module String) ~sort_by:lex_s "Firma" ~group:dsc)
        ~from:(add (module DateOpt) ~sort_by:dat_s "Von" ~group:visit)
        ~till:(add (module DateOpt) ~sort_by:dat_s "Bis" ~group:visit)
        ~rooms:(add (module IntOpt) ~sort_by:int_s "#Zimmer" ~group:visit)
        ~beds:(add (module IntOpt) ~sort_by:int_s "#Betten" ~group:visit)
        ~guests:(add (module IntOpt) ~sort_by:int_s "#Gäste" ~group:visit)
      |> List.rev

    let of_customer c : t =
      let summary = Customer.first_booking c
                    |> Option.map ~f:Booking.summarize
      in
      let period =
        Option.map ~f:Booking.Summary.period summary
        |> Option.join in
      let given = c.name.given
      and family = c.name.family
      and company = c.company.name
      and keyword = c.keyword
      and guests = summary |> Option.map ~f:Booking.Summary.guests
      and rooms = summary |> Option.map ~f:Booking.Summary.rooms
      and beds = summary |> Option.map ~f:Booking.Summary.beds
      and from = period |> Option.map ~f:Period.from
      and till = period |> Option.map ~f:Period.till
      in
      { given; family; company; keyword; from; till; guests; rooms; beds }

    let matches_pattern t pattern : bool =
      let substring = String.lowercase pattern in
      let matches s = String.is_substring ~substring (String.lowercase s) in
      matches t.keyword
  end

  let view ?on_click (m : Model.t Incr.t) =
    let%map m = m in
    let attrs = match on_click with
      | None -> []
      | Some ev ->
        Attr.[ on_click (fun _ -> ev)
             ; style (Css.create ~field:"cursor" ~value:"pointer") ]
    in
    let row_attrs = Rn_spec.Attrs.create ~attrs () in
    let cells =
      List.map Model.columns ~f:(fun col ->
          { Rn_spec.Cell.attrs = Rn_spec.Attrs.create ()
          ; node = Node.span [] [ Node.text (Column.get col m) ]
          })
    in
    { Rn_spec.row_attrs; cells }
end

module Model = struct
  type t =
    { table : Table.Model.t
    ; pattern : string
    }
  [@@deriving compare, fields]

  let create () =
    { table = Table.Model.create
          ~scroll_margin:(Incr_dom_widgets.Table.Margin.uniform 5.)
          ~scroll_region:Window
          ~float_header:None
          ~float_first_col:None
          ~height_guess:43.
          ()
    ; pattern = ""
    }
end

module Action = struct
  type t =
    | Table of Table.Action.t
    | Pattern of string
  [@@deriving sexp_of, variants]
end


let create_table customers pattern model ~old_model ~inject ~select =
  let%bind pattern = pattern in
  let columns = List.map ~f:Column.to_table_widget_column Row.Model.columns in
  let columns = Incr.const (List.mapi columns ~f:(fun i col -> i, col)) in
  let rows =
    Incr.Map.filter_mapi customers ~f:(fun ~key:_ ~data ->
        let row = Row.Model.of_customer data in
        Option.some_if (Row.Model.matches_pattern row pattern) row)
  and render_row = fun ~row_id ~row ->
    let on_click = select row_id in
    Row.view ~on_click row
  in Table.create model
    ~old_model
    ~rows
    ~columns
    ~render_row
    ~inject:(fun a -> inject (Action.Table a))
    ~attrs:[ Attr.classes ["table";"table-hover";"table-sm"] ]

let create ~model ~old_model ~inject ~select customers =
  let pattern = model >>| Model.pattern in
  let table =
    let model = model >>| Model.table
    and old_model = old_model >>| Model.table >>| Option.some in
    create_table customers pattern ~select ~old_model ~inject model
  in
  let%map table = table
  and model = model
  and size = customers >>| Storage.size
  in
  let apply_action =
    fun (a : Action.t) _state ~schedule_action ->
      match a with
      | Table a ->
        let schedule_action = Fn.compose schedule_action Action.table in
        let table = Component.apply_action ~schedule_action table a () in
        { model with table }
      | Pattern pattern -> { model with pattern }
  and view =
    let search =
      Node.input
        [ Attr.placeholder "Suche"
        ; Attr.type_ "text"
        (* TODO: Investigate whether the following creates a property or
           an attribute. We want an attribute, such that on return to view,
           the search field is filled. *)
        ; Attr.create "value" model.pattern
        ; Attr.on_input (fun _ev text -> inject (Pattern text))
        ] []
    in
    let counter =
      let s = Printf.sprintf "%d Kunden geladen." size in
      Node.text s
    and table = Component.view table
    in Node.div []
      [ Bs.row [search; counter]
      ; Bs.row [table] ]
  and update_visibility ~schedule_action : Model.t =
    let schedule_action = Fn.compose schedule_action Action.table in
    let table = Component.update_visibility table ~schedule_action in
    { model with table }
  and on_display _state ~schedule_action =
    let schedule_action = Fn.compose schedule_action Action.table in
    Component.on_display table ~schedule_action ()
  in Component.create ~update_visibility ~apply_action ~on_display model view
