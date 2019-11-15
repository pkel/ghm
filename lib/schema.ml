open Base
open Postgrest
open Resource

let parse ~f x =
  match f x with
  | Ok v -> Ok v
  | Error s -> Or_error.error_string s
;;

module Customers = struct
  type provide = Customer.t

  type return =
    { id : int
    ; data : Customer.t
    }
  [@@deriving of_yojson, compare]

  include Create (struct
    type a = provide
    type b = return
    type __provide = { data : Customer.t } [@@deriving to_yojson]

    let name = "api/customers"
    let select = [ "id"; "data" ]
    let provide data = __provide_to_yojson { data }
    let return = parse ~f:return_of_yojson
  end)

  open Column_creator

  let id = int "id"
  let id' = key id
  let modified = date "modified"
  let keyword = string "keyword"
end

module Bookings = struct
  type provide = Booking.t

  type return =
    { id : int
    ; data : Booking.t
    }
  [@@deriving of_yojson, compare]

  include Create (struct
    type a = provide
    type b = return
    type __provide = { data : Booking.t } [@@deriving to_yojson]

    let name = "api/bookings"
    let select = [ "id"; "data" ]
    let provide data = __provide_to_yojson { data }
    let return = parse ~f:return_of_yojson
  end)

  open Column_creator

  let id = int "id"
  let id' = key id
  let modified = date "modified"
  let arrival = date "arrival"
  let departure = date "departure"
end
