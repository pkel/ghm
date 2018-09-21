open Ghm
open Core_kernel
open Incr_dom
open Vdom
open Incr.Let_syntax
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
    let given = c |> Customer.given
    and family = c |> Customer.family
    and company = c |> Customer.company
    and keyword = c |> Customer.keyword
    and guests = summary |> Option.map ~f:Booking.Summary.guests
    and rooms = summary |> Option.map ~f:Booking.Summary.rooms
    and beds = summary |> Option.map ~f:Booking.Summary.beds
    and from = period |> Option.map ~f:Period.from
    and till = period |> Option.map ~f:Period.till
    in
    { given; family; company; keyword; from; till; guests; rooms; beds }
end

module Action = struct
  type t = unit [@@deriving sexp]
end

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
