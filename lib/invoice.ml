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

module Tax_logic = struct
  type t =
    | Simple of int
    | Split of
        { logic : (int * (Monetary.t -> Monetary.t)) list [@equal.ignore]
        ; descr : string
        }
  [@@deriving equal]

  let t = function
    | General19 -> Simple 19
    | Reduced7 -> Simple 7
    | Legacy rate -> Simple rate
    | Reduced7With3EuroDrinks19 ->
      Split
        { logic =
            [ (7, fun v -> Monetary.(v - of_int 3)); (19, fun _v -> Monetary.of_int 3) ]
        ; descr = "Enthält 3,00€ Getränke pro Nacht zu 19%, Rest 7%"
        }
  ;;
end

type position =
  { quantity : int
  ; description : string
  ; price : Monetary.t
  ; tax : tax
  }
[@@deriving yojson, fields, compare, sexp]

type t =
  { id : string option
  ; recipient : string
  ; invoice_date : Date.t option
  ; departure_date : Date.t option
  ; title : string
  ; intro : string
  ; positions : position list
  ; closing : string
  ; deposit : Monetary.t
  }
[@@deriving yojson, fields, compare, sexp]

module Compat_t_0 = struct
  (** old invoice; before introducing departure date *)

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
  [@@deriving of_yojson]

  let upgrade t0 =
    { id = t0.id
    ; recipient = t0.recipient
    ; invoice_date = t0.date
    ; departure_date = None
    ; title = t0.title
    ; intro = t0.intro
    ; positions = t0.positions
    ; closing = t0.closing
    ; deposit = t0.deposit
    }
  ;;
end

let of_yojson json =
  match of_yojson json with
  | Ok t -> Ok t
  | Error msg ->
    (match Compat_t_0.of_yojson json with
    | Ok t0 -> Ok (Compat_t_0.upgrade t0)
    | Error _ -> Error msg)
;;

let empty_position =
  { quantity = 1; description = ""; price = Monetary.zero; tax = General19 }
;;

let empty =
  { recipient = ""
  ; title = ""
  ; id = None
  ; invoice_date = None
  ; departure_date = None
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

module Tax = struct
  type tax_row =
    { label : string
    ; description : [ `Rate of int | `Split of string ]
    ; mutable value : float (* = zero, for Split rules *)
    }

  let init_tax_table t =
    (* create assoc: tax_rule -> tax_row *)
    List.concat_map
      ~f:(fun pos ->
        match Tax_logic.t pos.tax with
        | Simple i -> [ Tax_logic.Simple i ]
        | Split { logic; _ } as this ->
          this :: List.map ~f:(fun (rate, _) -> Tax_logic.Simple rate) logic)
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
                   | Simple rate -> `Rate rate
                   | Split { descr; _ } -> `Split descr)
               ; value = 0.
               }
             in
             (rule, row) :: assoc))
         ~init:[]
    |> List.rev
  ;;

  let calculate_tax_into table t =
    (* apply taxation rules into tax table; mutation *)
    t.positions
    |> List.map ~f:(fun pos ->
           let rule = Tax_logic.t pos.tax
           and value = Monetary.(times pos.quantity pos.price) in
           value, rule)
    |> List.concat_map ~f:(fun (value, rule) ->
           match rule with
           | Simple rate -> [ value, rate ]
           | Split { logic; _ } -> List.map ~f:(fun (rate, get) -> get value, rate) logic)
    |> List.iter ~f:(fun (value, rate) ->
           let row =
             List.Assoc.find_exn ~equal:Tax_logic.equal table (Tax_logic.Simple rate)
           in
           let rate = float_of_int rate /. 100. in
           let value = Monetary.to_float value in
           let add_v = value *. rate /. (1. +. rate) in
           row.value <- row.value +. add_v)
  ;;

  let tax t =
    (* calculate taxes, return rows of tax table and (tax -> string) tax class lookup *)
    let table = init_tax_table t in
    let () = calculate_tax_into table t in
    let tax_rows =
      List.map
        ~f:(fun (_, row) ->
          row.label, row.description, Option.value_exn (Monetary.of_float row.value))
        table
    and lookup tax =
      List.Assoc.find ~equal:Tax_logic.equal table (Tax_logic.t tax)
      |> Option.map ~f:(fun r -> r.label)
    in
    tax_rows, lookup
  ;;
end
