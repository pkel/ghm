open Core_kernel
module Date = Date_yojson

type t =
  { id : string option
  ; recipient : string
  ; date : Date.t option
  ; title : string
  ; intro : string
  ; positions : position list
  ; closing : string
  ; deposit : Monetary.t
  }

and position =
  { quantity : int
  ; description : string
  ; price : Monetary.t
  ; tax : int (** percent *)
  }
[@@deriving yojson, fields, compare, sexp]

let empty_position =
  { quantity = 1; description = ""; price = Monetary.zero; tax = 16 (* tax = 19 *) }
;;

let empty =
  { recipient = ""
  ; title = ""
  ; id = None
  ; date = None
  ; positions = []
  ; deposit = Monetary.zero
  ; intro = ""
  ; closing = ""
  }
;;

type summary =
  { sum : Monetary.t
  ; included_tax : (int * Monetary.t) list
  }

let summary : t -> summary =
  let add tax x txmap =
    let was = Int.Map.find txmap tax |> Option.value ~default:0.
    and tax' = float_of_int tax /. 100. in
    let data = was +. (x *. tax' /. (1. +. tax')) in
    Int.Map.set ~key:tax ~data txmap
  and monetary x = Monetary.of_float x |> Option.value_exn in
  fun t ->
    let sum, txmap =
      List.fold_left
        ~f:(fun (sum, txmap) pos ->
          let x = Monetary.(times pos.quantity pos.price |> to_float) in
          sum +. x, add pos.tax x txmap)
        ~init:(0., Int.Map.empty)
        t.positions
    in
    { sum = monetary sum; included_tax = Int.Map.(map ~f:monetary txmap |> to_alist) }
;;
