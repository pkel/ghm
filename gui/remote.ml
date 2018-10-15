open Core_kernel
open Ghm

type 'a page = { this: 'a;
                 is_first: bool;
                 load_next: (unit -> unit) option}

type 'a order = Asc of 'a | Desc of 'a

let string_of_order f = function
  | Asc key -> sprintf "%s.asc" (f key)
  | Desc key -> sprintf "%s.desc" (f key)

include struct
  [@@@warning "-39"]

  type entry = {id: int [@key "customer_id"]; data: Customer.t}
  [@@deriving of_yojson {strict= false}]

  type response = entry list [@@deriving of_yojson]
end

let url_with_params url params =
  let params =
    List.map ~f:(fun (l, r) -> sprintf "%s=%s" l r) params
    |> String.concat ~sep:"&"
    |> function "" -> "" | s -> "?" ^ s
  in
  sprintf "/api/%s%s" url params

let data_only json = `Assoc [("data", json)]

let parse f x =
  match f x with Ok v -> Ok v | Error s -> Or_error.error_string s

let single = function
  | [e] -> Ok e
  | _ -> Or_error.error_string "singleton list expected"

module Customer = struct
  type t = Customer.t

  type id = int

  let get id =
    let url =
      url_with_params "customers" [("customer_id", sprintf "eq.%i" id)]
    in
    Request.(
      create ~v:GET ~url |> want_json
      |> conv_resp ~f:(parse response_of_yojson)
      |> conv_resp ~f:single
      |> map_resp ~f:(fun x -> x.data))

  let post =
    let url = url_with_params "customers" [] in
    Request.(
      create ~v:POST ~url
      |> prefer "return=representation"
      |> want_json |> give_json
      |> conv_resp ~f:(parse response_of_yojson)
      |> conv_resp ~f:single
      |> map_resp ~f:(fun x -> (x.id, x.data))
      |> map_body ~f:data_only
      |> map_body ~f:Customer.to_yojson)

  let patch id =
    let url =
      url_with_params "customers" [("customer_id", sprintf "eq.%i" id)]
    in
    Request.(
      create ~v:PATCH ~url
      |> prefer "return=representation"
      |> want_json |> give_json
      |> conv_resp ~f:(parse response_of_yojson)
      |> conv_resp ~f:single
      |> map_resp ~f:(fun x -> x.data)
      |> map_body ~f:data_only
      |> map_body ~f:Customer.to_yojson)
end

module Customers = struct
  type t = Customer.t Int.Map.t

  type key = Id | Modified

  let string_of_key = function Id -> "customer_id" | Modified -> "modified"

  let string_of_order = string_of_order string_of_key

  let get_page ?offset ?sort ~n =
    if n < 1 then raise (Invalid_argument "get_page: n > 0 expected") ;
    let url =
      let sort = Option.map ~f:(fun k -> ("order", string_of_order k)) sort
      and limit = Some ("limit", string_of_int n)
      and offset =
        Option.map ~f:(fun o -> ("offset", string_of_int o)) offset
      in
      let params = List.filter_opt [sort; limit; offset] in
      url_with_params "customers" params
    in
    Request.(
      create ~v:GET ~url |> want_json
      |> conv_resp ~f:(parse response_of_yojson)
      |> map_resp ~f:(List.map ~f:(fun r -> (r.id, r.data)))
      |> conv_resp ~f:Int.Map.of_alist_or_error
      |> map_resp ~f:(fun this ->
          let is_first = Option.is_some offset in
          let load_next =
            if Int.Map.length this < n then None
            else None (** TODO: pagination is broken *)
            (* let offset = Option.value ~default:0 offset + n in *)
            (* Some (fun () -> Request.send' (get_page ~offset ?sort ~n) )) *)
          in {this; load_next; is_first}
        ))

  let get_page = get_page ?offset:None
end
