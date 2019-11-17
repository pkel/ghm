open Ghm
open Core_kernel
open Incr_dom
open Vdom
open Incr.Let_syntax

(* TODO: Get rid of unnecessary code from ts_gui example *)

module RowId = Int

module Table = struct
  open Incr_dom_partial_render
  include Table.Make (RowId) (Core_kernel.Int) (Table.Default_sort_spec)
end

module Form = Incr_dom_widgets.Form

module Column = struct
  open Core_kernel
  open Incr_dom
  module Sort_key = Incr_dom_partial_render.Table.Default_sort_spec.Sort_key

  module type CONTENTS = sig
    type t

    val to_string : t -> string
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
    (module struct
      type nonrec row = row

      module Contents = Contents

      let name = name
      let group = group
      let get = get
      let sort_by = sort_by
    end : T
      with type row = row)
  ;;

  let of_field
      (type contents)
      field
      ?name
      ?group
      ?sort_by
      (module Contents : CONTENTS with type t = contents)
    =
    let name = Option.value ~default:(Field.name field) name in
    create ~name ?group ?sort_by (module Contents) ~get:(Field.get field)
  ;;

  let name (type row) (module T : T with type row = row) = T.name
  let group (type row) (module T : T with type row = row) = T.group

  let get (type row) (module T : T with type row = row) row =
    T.Contents.to_string (T.get row)
  ;;

  let sort_by (type row) (module T : T with type row = row) row = T.sort_by (T.get row)

  let to_table_widget_column t =
    let name = name t in
    let group = group t in
    let sort_by _row_id row = sort_by t row in
    Table.Column.create ~header:(Vdom.Node.text name) ~sort_by ?group ()
  ;;
end

module Row = struct
  module Rn_spec = Incr_dom_partial_render.Row_node_spec
  module Sort_key = Incr_dom_partial_render.Table.Default_sort_spec.Sort_key

  module IntOpt = struct
    type t = int option

    let to_string t = Option.map ~f:string_of_int t |> Option.value ~default:""
    let sort_key x = Option.value ~default:0 x |> Int63.of_int |> Sort_key.Integer
  end

  module DateOpt = struct
    type t = Date.t option

    let string_of_date d = Localize.date d
    let to_string t = Option.map ~f:string_of_date t |> Option.value ~default:""

    let sort_key x =
      Sort_key.String
        (Option.map ~f:Date.to_string_iso8601_basic x |> Option.value ~default:"")
    ;;
  end

  module Model = struct
    type t =
      { id : int
      ; keyword : string
      ; given : string
      ; family : string
      ; company : string
      ; (* last booking *)
        from : Date.t option
      ; till : Date.t option
      ; rooms : string
      ; guests : int option
      }
    [@@deriving compare, fields]

    let columns =
      let append f list field = f field :: list in
      let add ?group ?sort_by m name =
        append (fun field -> Column.of_field field m ~name ?group ?sort_by)
      in
      let dsc = "Stammdaten"
      and visit = "Letzter Aufenthalt" in
      let lex_s x = Sort_key.String x in
      let dat_s = DateOpt.sort_key in
      let int_s = IntOpt.sort_key in
      Fields.fold
        ~init:[]
        ~id:(fun l _ -> l)
        ~keyword:(add (module String) ~sort_by:lex_s "Schlüsselwort")
        ~given:(add (module String) ~sort_by:lex_s "Vorname" ~group:dsc)
        ~family:(add (module String) ~sort_by:lex_s "Nachname" ~group:dsc)
        ~company:(add (module String) ~sort_by:lex_s "Firma" ~group:dsc)
        ~from:(add (module DateOpt) ~sort_by:dat_s "Von" ~group:visit)
        ~till:(add (module DateOpt) ~sort_by:dat_s "Bis" ~group:visit)
        ~rooms:(add (module String) ~sort_by:lex_s "Zimmer" ~group:visit)
        ~guests:(add (module IntOpt) ~sort_by:int_s "Gäste" ~group:visit)
      |> List.rev
    ;;

    let of_customer ~id c : t =
      let booking = Customer.first_booking c in
      let summary = Option.map ~f:Booking.summarize booking in
      let period = Option.map ~f:Booking.period booking in
      let given = c.name.given
      and family = c.name.family
      and company = c.company.name
      and keyword = c.keyword
      and guests = summary |> Option.map ~f:Booking.Summary.guests
      and rooms =
        summary
        |> Option.map ~f:Booking.Summary.rooms
        |> Option.map ~f:(String.concat ~sep:", ")
        |> Option.value ~default:""
      and from = period |> Option.map ~f:Period.from
      and till = period |> Option.map ~f:Period.till in
      { id; given; family; company; keyword; from; till; guests; rooms }
    ;;
  end

  let view (m : Model.t Incr.t) =
    let%map m = m in
    let row_attrs =
      Attr.
        [ on_click Nav.(nav (Customer (Id m.id, CData)))
        ; style (Css_gen.create ~field:"cursor" ~value:"pointer")
        ]
    in
    let cells =
      List.map Model.columns ~f:(fun col ->
          { Rn_spec.Cell.attrs = []
          ; node = Node.span [] [ Node.text (Column.get col m) ]
          })
    in
    { Rn_spec.row_attrs; cells }
  ;;
end

module Model = struct
  type t = { table : Table.Model.t } [@@deriving compare, fields]

  let create () =
    { table =
        Table.Model.create
          ~scroll_margin:(Incr_dom_partial_render.Table.Margin.uniform 5.)
          ~scroll_region:Window
          ~float_header:None
          ~float_first_col:None
          ~height_guess:43.
          ()
    }
  ;;

  module Row : sig
    type t [@@deriving compare]

    val of_customer : id:int -> Customer.t -> t
  end
  with type t = Row.Model.t =
    Row.Model
end

module Action = struct
  type t = Table of Table.Action.t [@@deriving sexp_of, variants]
end

let create_table rows model ~old_model ~inject =
  let columns = List.map ~f:Column.to_table_widget_column Row.Model.columns in
  let columns = Incr.const (List.mapi columns ~f:(fun i col -> i, col)) in
  let render_row ~row_id:_ ~row = Row.view row in
  Table.create
    model
    ~old_model
    ~rows
    ~columns
    ~render_row
    ~inject:(fun a -> inject (Action.Table a))
    ~attrs:[ Attr.classes [ "table"; "table-hover"; "table-sm" ] ]
;;

let create ~model ~old_model ~inject rows =
  let table =
    let model = model >>| Model.table
    and old_model = old_model >>| Model.table >>| Option.some in
    create_table rows ~old_model ~inject model
  in
  let%map table = table
  and model = model in
  let apply_action (a : Action.t) _state ~schedule_action =
    match a with
    | Table a ->
      let schedule_action = Fn.compose schedule_action Action.table in
      let table' = Component.apply_action ~schedule_action table a () in
      Model.{ table = table' }
  and view = Component.view table
  and update_visibility ~schedule_action : Model.t =
    let schedule_action = Fn.compose schedule_action Action.table in
    let table' = Component.update_visibility table ~schedule_action in
    Model.{ table = table' }
  and on_display _state ~schedule_action =
    let schedule_action = Fn.compose schedule_action Action.table in
    Component.on_display table ~schedule_action ()
  in
  Component.create ~update_visibility ~apply_action ~on_display model view
;;
