open Core_kernel
module Date = Date_yojson

type tax =
  | General19
  | Reduced7
  | Reduced7With3EuroDrinks19
  | Legacy of int
[@@deriving yojson, compare, sexp]

let tax_of_yojson json =
  match json with
  | `Int 19 -> Ok General19
  | `Int 7 -> Ok Reduced7
  | `Int i -> Ok (Legacy i)
  | other -> tax_of_yojson other
;;

let tax_rule = function
  | General19 -> `Simple 19
  | Reduced7 -> `Simple 7
  | Legacy rate -> `Simple rate
  | Reduced7With3EuroDrinks19 ->
    `Split
      ( [ (7, fun v -> Monetary.(v - of_int 3)); (19, fun _v -> Monetary.of_int 3) ]
      , "Enthält 3,00€ Getränke pro Nacht zu 19%, Rest 7%" )
;;

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
  ; tax : tax
  }
[@@deriving yojson, fields, compare, sexp]

let empty_position =
  { quantity = 1; description = ""; price = Monetary.zero; tax = General19 }
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

let sum t =
  List.fold_left
    ~f:(fun acc pos -> Monetary.(times pos.quantity pos.price + acc))
    ~init:Monetary.zero
    t.positions
;;

module Tax_row = struct
  type t =
    { label : string
    ; description : [ `Rate of int | `Split of string ]
    ; mutable value : float (* = zero, for Split rules *)
    }
end

let tabulate_tax t =
  let open Tax_row in
  let table =
    (* 1. initialize all rows *)
    List.concat_map
      ~f:(fun pos ->
        match tax_rule pos.tax with
        | `Simple i -> [ `Simple i ]
        | `Split (logic, _descr) as this ->
          this :: List.map ~f:(fun (rate, _) -> `Simple rate) logic)
      t.positions
    |> List.sort ~compare:Stdlib.compare
    |> List.fold_left
         ~f:(fun assoc rule ->
           if List.Assoc.mem ~equal:( = ) assoc rule
           then assoc
           else (
             let row =
               { label = List.length assoc + 65 |> Char.unsafe_of_int |> Char.to_string
               ; description =
                   (match rule with
                   | `Simple rate -> `Rate rate
                   | `Split (_logic, descr) -> `Split descr)
               ; value = 0.
               }
             in
             (rule, row) :: assoc))
         ~init:[]
    |> List.rev
  in
  let () =
    (* 2. apply taxation rules *)
    t.positions
    |> List.map ~f:(fun pos ->
           let rule = tax_rule pos.tax
           and value = Monetary.(times pos.quantity pos.price) in
           value, rule)
    |> List.concat_map ~f:(fun (value, rule) ->
           match rule with
           | `Simple rate -> [ value, rate ]
           | `Split (logic, _descr) ->
             List.map ~f:(fun (rate, get) -> get value, rate) logic)
    |> List.iter ~f:(fun (value, rate) ->
           let row = List.Assoc.find_exn ~equal:( = ) table (`Simple rate) in
           let rate = float_of_int rate /. 100. in
           let value = Monetary.to_float value in
           let add_v = value *. rate /. (1. +. rate) in
           row.value <- row.value +. add_v)
  in
  List.map
    ~f:(fun (_, row) ->
      row.label, row.description, Option.value_exn (Monetary.of_float row.value))
    table
;;
