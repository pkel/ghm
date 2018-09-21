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

type 'a t = (module T with type row = 'a)

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
