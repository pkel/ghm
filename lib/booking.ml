open Core_kernel
module Date = Date_yojson

type t =
  { period : Period.t
  ; deposit_asked : Monetary.t option
  ; deposit_got : Monetary.t option
  ; tax_free : bool
  ; note : string
  ; guests : guest list
  ; allocs : alloc list
  ; invoice : Invoice.t option [@default None]
  }

and guest =
  { given : string
  ; family : string
  ; born : Date.t option
  }

and alloc =
  { room : string
  ; price_per_bed : Monetary.t
  ; beds : int
  ; description : string
  }
[@@deriving yojson, fields, compare, sexp]

let empty ~period =
  { period
  ; deposit_asked = None
  ; deposit_got = None
  ; tax_free = false
  ; note = ""
  ; guests = []
  ; allocs = []
  ; invoice = None
  }
;;

let empty_guest = { given = ""; family = ""; born = None }

let empty_alloc =
  { room = ""
  ; price_per_bed = Monetary.of_float 53. |> Option.value ~default:Monetary.zero
  ; beds = 2
  ; description = "Doppelzimmer"
  }
;;

type _booking = t

module Rooms : sig
  type t [@@deriving compare]

  val of_booking : _booking -> t
  val to_string : t -> string option
  val first : t -> string option
end = struct
  type el =
    | Num of int
    | Txt of string
    | Nil
  [@@deriving compare]

  type t = el list [@@deriving compare]

  let parse_el s : el =
    let s = String.strip s in
    if String.is_empty s
    then Nil
    else (
      match Int.of_string s with
      | i -> Num i
      | exception _ -> Txt s)
  ;;

  let of_booking b : t =
    List.map b.allocs ~f:(fun a -> parse_el a.room)
    |> List.dedup_and_sort ~compare:compare_el
  ;;

  let el_to_string = function
    | Num i -> Some (string_of_int i)
    | Txt s -> Some s
    | Nil -> None
  ;;

  let to_string l : string option =
    List.filter_map l ~f:el_to_string
    |> function
    | [] -> None
    | l -> Some (String.concat ~sep:", " l)
  ;;

  let first = function
    | [] -> None
    | hd :: _ -> el_to_string hd
  ;;
end

module Summary = struct
  type t =
    { rooms : Rooms.t
    ; guests : int
    ; tax_payers : int
    }
  [@@deriving fields]

  let of_booking b : t =
    (* Age on arrival >= 15 makes you a tax payer *)
    let tax_cutoff = Date.add_years (Period.till b.period) (-15) in
    let guests = List.length b.guests
    and tax_payers =
      List.count b.guests ~f:(fun g ->
          match g.born with
          | None -> true
          | Some d -> Date.compare tax_cutoff d > 0)
    and rooms = Rooms.of_booking b in
    { rooms; guests; tax_payers }
  ;;
end

let room_descriptions =
  [ "Einzelzimmer"
  ; "Doppelzimmer"
  ; "Dreibettzimmer"
  ; "Vierbettzimmer"
  ; "Doppel- als Einzelzimmer"
  ; "Kurtaxe"
  ]
;;

let pp_alloc fmt alloc =
  let a, b = if alloc.beds < 2 then "Person", "für" else "Personen", "zu je" in
  Format.fprintf
    fmt
    "%d %s im %s %s %a € pro Nacht"
    alloc.beds
    a
    alloc.description
    b
    Monetary.print
    alloc.price_per_bed
;;

let string_of_alloc x = Format.asprintf "%a" pp_alloc x
