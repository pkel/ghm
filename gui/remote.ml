open Core_kernel
open Ghm

type 'a order = Asc of 'a | Desc of 'a

let string_of_order f = function
  | Asc key -> sprintf "%s.asc" (f key)
  | Desc key -> sprintf "%s.desc" (f key)

include struct
  [@@@warning "-39"]

  type entry = {id: int [@key "customer_id"]; data: Customer.t}
  [@@deriving of_yojson {strict= false}]

  type many = entry list [@@deriving of_yojson]
end

let base_url = sprintf "/api/%s"

let data_only json = `Assoc [("data", json)]

let parse f x =
  match f x with Ok v -> Ok v | Error s -> Or_error.error_string s

let get_single_customer =
  Request.(
    create ~url:(base_url "customers")
    |> want_json
    |> Request.header ~key:"Accept" ~value:"application/vnd.pgrst.object+json"
    |> conv_resp ~f:(parse entry_of_yojson))

let get_all_customers =
  Request.(
    create ~url:(base_url "customers")
    |> want_json
    |> conv_resp ~f:(parse many_of_yojson))

module Customer = struct
  type t = Customer.t

  type id = int

  let get id =
    Request.(
      get_single_customer
      |> param ~key:"customer_id" ~value:(sprintf "eq.%i" id)
      |> map_resp ~f:(fun x -> x.data))

  let give_single =
    Request.(
      give_json get_single_customer
      |> map_body ~f:data_only
      |> map_body ~f:Customer.to_yojson)

  let post =
    Request.(
      verb POST give_single
      |> header ~key:"Prefer" ~value:"return=representation"
      |> map_resp ~f:(fun x -> (x.id, x.data)))

  let patch id =
    Request.(
      verb PATCH give_single
      |> param ~key:"customer_id" ~value:(sprintf "eq.%i" id)
      |> header ~key:"Prefer" ~value:"return=representation"
      |> map_resp ~f:(fun x -> x.data))
end

module Customers = struct
  type t = (int * Customer.t) list

  type key = Id | Modified

  let string_of_key = function Id -> "customer_id" | Modified -> "modified"

  type filter = Keyword of string

  let string_of_filter = function Keyword s -> sprintf "ilike.%%%s%%" s

  let string_of_order = string_of_order string_of_key

  let string_of_sort l =
    List.map ~f:string_of_order l |> String.concat ~sep:","

  let get ?offset ?limit ?(sort = [Desc Modified; Desc Id]) ?filter () =
    let opt_param to_string key = function
      | Some i -> Request.param ~key ~value:(to_string i)
      | None -> Fn.id
    in
    Request.(
      get_all_customers
      |> opt_param string_of_int "offset" offset
      |> opt_param string_of_int "limit" limit
      |> param ~key:"order" ~value:(string_of_sort sort)
      |> opt_param string_of_filter "keyword" filter
      |> map_resp ~f:(List.map ~f:(fun r -> (r.id, r.data))))
end
