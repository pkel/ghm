open Base
open Postgrest
open Resource

let parse ~f x =
  match f x with
  | Ok v -> Ok v
  | Error s -> Or_error.error_string s
;;

module Customers = struct
  include Create (struct
    type provide = Customer.t

    type return =
      { id : int
      ; data : Customer.t
      }
    [@@deriving of_yojson]

    let name = "customers"
    let select = [ "id"; "data" ]
    let provide = Customer.to_yojson
    let return = parse ~f:return_of_yojson
  end)

  open Column_creator

  let id = int "id"
  let modified = date "modified"
  let keyword = string "keyword"
end

module Bookings = struct
  include Create (struct
    type provide = Booking.t

    type return =
      { id : int
      ; data : Booking.t
      }
    [@@deriving of_yojson]

    let name = "bookings"
    let select = [ "id"; "data" ]
    let provide = Booking.to_yojson
    let return = parse ~f:return_of_yojson
  end)

  open Column_creator

  let id = int "id"
  let modified = date "modified"
  let arrival = date "arrival"
  let departure = date "departure"
end
