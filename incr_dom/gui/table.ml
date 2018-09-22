open Incr_dom_widgets
module RowId = struct
  include Core_kernel.Int
  let compare a b = compare b a
end
include Table.Make (RowId) (Core_kernel.Int) (Table.Default_sort_spec)
