open Core_kernel

type t = Customer.t Int.Map.t [@@deriving compare, sexp]

let empty = Int.Map.empty

let save = Int.Map.set

let load = Int.Map.find

let size = Int.Map.length

let pp_hum fmt t = Sexplib.Sexp.pp_hum fmt (sexp_of_t t)

let of_string s = Sexp.of_string s |> t_of_sexp

include struct
  [@@@warning "-39"]

  type db_entry = {customer_id: int; data: Customer.t} [@@deriving yojson]
end

let to_yojson t =
  Caml.([%to_yojson: db_entry list])
    ( Int.Map.to_alist t
    |> List.map ~f:(fun (customer_id, data) -> {customer_id; data}) )

let of_yojson y =
  Result.map
    ([%of_yojson: db_entry list] y)
    ~f:
      (List.fold_left ~init:empty ~f:(fun t {customer_id= key; data} ->
           save t ~key ~data ))

let chunks ?(ascending = false) ?firstsize ~size t =
  let fst = Option.value ~default:size firstsize in
  let fold = if ascending then Int.Map.fold else Int.Map.fold_right in
  if size <= 0 then raise (Invalid_argument "size must be grater zero") ;
  let l, _, c =
    fold t
      ~f:(fun ~key ~data (l, sz, c) ->
        if sz = 0 then (c :: l, size, Int.Map.singleton key data)
        else (l, sz - 1, save c ~key ~data) )
      ~init:([], fst, empty)
  in
  c :: l |> List.rev

let add_chunk ~chunk t =
  match Int.Map.append ~lower_part:chunk ~upper_part:t with
  | `Ok t -> t
  | `Overlapping_key_ranges -> (
    match Int.Map.append ~lower_part:t ~upper_part:chunk with
    | `Ok t -> t
    | `Overlapping_key_ranges ->
        Int.Map.merge t chunk ~f:(fun ~key -> function
          | `Both _ ->
              let msg = Printf.sprintf "chunk overlapping in key %d" key in
              raise (Invalid_argument msg)
          | `Left x | `Right x -> Some x ) )
