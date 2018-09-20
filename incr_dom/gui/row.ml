open Ghm
open Core_kernel
module Sort_key = Incr_dom_widgets.Table.Default_sort_spec.Sort_key

module Model = struct
  type t = Customer.t [@@deriving compare]

  let columns =
    let append f list field = f field :: list in
    let add ?group ?(editable = false) ?focus_on_edit ?sort_by m name w =
      append (fun field ->
          w, Column.of_field field m ~editable ~name
            ?group ?sort_by ?focus_on_edit)
    in
    let lex_s x = Sort_key.String x in
    let ign acc _f = acc in
    Customer.Fields.fold
      ~init:[]
      ~title:ign
      ~title_letter:ign

      ~given:(add (module String) ~sort_by:lex_s "Vorname" 1)
      ~second:ign
      ~family:(add (module String) ~sort_by:lex_s "Nachname" 2)

      ~company:(add (module String) ~sort_by:lex_s "Firma" 3)
      ~company_address:ign

      ~street:ign
      ~street_number:ign
      ~postal_code:ign
      ~city:ign
      ~country:ign
      ~country_code:ign

      ~phone:ign
      ~phone2:ign
      ~mobile:ign
      ~fax:ign
      ~fax2:ign
      ~mail:ign
      ~mail2:ign
      ~web:ign

      ~keyword:(add (module String) ~sort_by:lex_s "Schlüssel" 0)
      ~note:ign
    |> List.sort ~compare:(fun (w1,_) (w2,_) -> Int.compare w1 w2)
    |> List.map ~f:snd
end

module Action = struct
  type t = unit [@@deriving sexp]
end

open Incr_dom
open Vdom
open Incr.Let_syntax
module Rn_spec = Incr_dom_widgets.Row_node_spec

let view (m : Model.t Incr.t) =
  let%map m = m in
  let row_attrs = Rn_spec.Attrs.create () in
  let cells =
    List.map Model.columns ~f:(fun col ->
        { Rn_spec.Cell.attrs = Rn_spec.Attrs.create ()
        ; node = Node.span [] [ Node.text (Column.get col m) ]
        })
  in
  { Rn_spec.row_attrs; cells }
