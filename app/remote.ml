open Core_kernel
open Async_kernel
open Ghm

let base_url = sprintf "/api/%s"
let data_only json = `Assoc [ "data", json ]

let parse f x =
  match f x with
  | Ok v -> Ok v
  | Error s -> Or_error.error_string s
;;

type connection = { mutable token : string }
type ('a, 'b) request = ('a, 'b) Request.t

let finalize c = Request.bearer ~token:c.token
let get_token = Request.(create ~url:"?action=token" |> want_text)

let refresh c () =
  Request.XHR.Deferred.send' get_token
  >>| function
  | Ok t -> c.token <- t
  | Error _ -> ()
;;

(* TODO think about this case *)

let keep_fresh c =
  (* Token valid for 300s *)
  every (Time_ns.Span.of_sec 240.) (fun () -> don't_wait_for (refresh c ()))
;;

let connect () =
  Request.XHR.Deferred.send' get_token
  >>| function
  | Ok token ->
    let c = { token } in
    keep_fresh c;
    Some c
  | Error _ -> None
;;

type 'a order =
  | Asc of 'a
  | Desc of 'a

let string_of_order f = function
  | Asc key -> sprintf "%s.asc" (f key)
  | Desc key -> sprintf "%s.desc" (f key)
;;

module type RESOURCE = sig
  type t
  type id
  type key
  type filter

  module S : sig
    val get : id -> (unit, t) request
    val post : (t, id * t) request
    val patch : id -> (t, t) request
    val delete : id -> (unit, unit) request
  end

  module M : sig
    type nonrec t = (id * t) list

    val get
      :  ?offset:int
      -> ?limit:int
      -> ?sort:key order list
      -> ?filter:filter
      -> unit
      -> (unit, t) request
  end
end

type customer_key =
  | Id
  | Modified

type customer_filter = Keyword of string

module Customer = struct
  type t = Customer.t

  module S = struct
    type foreign =
      { id : int
      ; data : Customer.t
      }
    [@@deriving of_yojson { strict = false }]

    let get_single_customer =
      Request.(
        create ~url:(base_url "customers")
        |> want_json
        |> Request.header ~key:"Accept" ~value:"application/vnd.pgrst.object+json"
        |> conv_resp ~f:(parse foreign_of_yojson))
    ;;

    let delete id =
      Request.(
        create ~url:(base_url "customers")
        |> param ~key:"id" ~value:(sprintf "eq.%i" id)
        |> verb DELETE)
    ;;

    let get id =
      Request.(
        get_single_customer
        |> param ~key:"id" ~value:(sprintf "eq.%i" id)
        |> map_resp ~f:(fun x -> x.data))
    ;;

    let give_single =
      Request.(
        give_json get_single_customer
        |> map_body ~f:data_only
        |> map_body ~f:Customer.to_yojson)
    ;;

    let post =
      Request.(
        verb POST give_single
        |> header ~key:"Prefer" ~value:"return=representation"
        |> map_resp ~f:(fun x -> x.id, x.data))
    ;;

    let patch id =
      Request.(
        verb PATCH give_single
        |> param ~key:"id" ~value:(sprintf "eq.%i" id)
        |> header ~key:"Prefer" ~value:"return=representation"
        |> map_resp ~f:(fun x -> x.data))
    ;;
  end

  module M = struct
    open S

    type nonrec t = (int * t) list
    type foreign = S.foreign list [@@deriving of_yojson]

    let string_of_key = function
      | Id -> "id"
      | Modified -> "modified"
    ;;

    let string_of_filter = function
      | Keyword s -> sprintf "ilike.%s" s
    ;;

    let string_of_order = string_of_order string_of_key
    let string_of_sort l = List.map ~f:string_of_order l |> String.concat ~sep:","

    let get_all_customers =
      Request.(
        create ~url:(base_url "customers")
        |> want_json
        |> conv_resp ~f:(parse foreign_of_yojson))
    ;;

    let get ?offset ?limit ?(sort = [ Desc Modified; Desc Id ]) ?filter () =
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
        |> map_resp ~f:(List.map ~f:(fun r -> r.id, r.data)))
    ;;
  end
end

type booking_key =
  | Id
  | Modified
  | Arrival
  | Departure

type booking_filter = None

(* TODO: implement functorial interface to postgrest? *)
module Booking = struct
  type t = Booking.t

  module S = struct
    type foreign =
      { id : int
      ; data : Booking.t
      }
    [@@deriving of_yojson { strict = false }]

    let get_single_customer =
      Request.(
        create ~url:(base_url "bookings")
        |> want_json
        |> Request.header ~key:"Accept" ~value:"application/vnd.pgrst.object+json"
        |> conv_resp ~f:(parse foreign_of_yojson))
    ;;

    let delete id =
      Request.(
        create ~url:(base_url "bookings")
        |> param ~key:"id" ~value:(sprintf "eq.%i" id)
        |> verb DELETE)
    ;;

    let get id =
      Request.(
        get_single_customer
        |> param ~key:"id" ~value:(sprintf "eq.%i" id)
        |> map_resp ~f:(fun x -> x.data))
    ;;

    let give_single =
      Request.(
        give_json get_single_customer
        |> map_body ~f:data_only
        |> map_body ~f:Booking.to_yojson)
    ;;

    let post =
      Request.(
        verb POST give_single
        |> header ~key:"Prefer" ~value:"return=representation"
        |> map_resp ~f:(fun x -> x.id, x.data))
    ;;

    let patch id =
      Request.(
        verb PATCH give_single
        |> param ~key:"id" ~value:(sprintf "eq.%i" id)
        |> header ~key:"Prefer" ~value:"return=representation"
        |> map_resp ~f:(fun x -> x.data))
    ;;
  end

  module M = struct
    open S

    type nonrec t = (int * t) list
    type foreign = S.foreign list [@@deriving of_yojson]

    let string_of_key = function
      | Id -> "id"
      | Modified -> "modified"
      | Arrival -> "arrival"
      | Departure -> "departure"
    ;;

    let string_of_filter = function
      | None -> ""
    ;;

    let string_of_order = string_of_order string_of_key
    let string_of_sort l = List.map ~f:string_of_order l |> String.concat ~sep:","

    let get_all_bookings =
      Request.(
        create ~url:(base_url "bookings")
        |> want_json
        |> conv_resp ~f:(parse foreign_of_yojson))
    ;;

    let get ?offset ?limit ?(sort = [ Desc Modified; Desc Id ]) ?filter () =
      let opt_param to_string key = function
        | Some i -> Request.param ~key ~value:(to_string i)
        | None -> Fn.id
      in
      Request.(
        get_all_bookings
        |> opt_param string_of_int "offset" offset
        |> opt_param string_of_int "limit" limit
        |> param ~key:"order" ~value:(string_of_sort sort)
        |> opt_param string_of_filter "keyword" filter
        |> map_resp ~f:(List.map ~f:(fun r -> r.id, r.data)))
    ;;
  end
end
