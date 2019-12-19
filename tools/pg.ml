include Request
include Postgrest.Query
include Postgrest.Make (Request)

module Schema = struct
  open Ghm
  open Base
  open Postgrest.Resource

  let parse ~f x =
    match f x with
    | Ok v -> Ok v
    | Error s -> Or_error.error_string s
  ;;

  module Customers = struct
    type provide =
      { data : Customer.t
      ; modified : string
      ; created : string
      }
    [@@deriving yojson, compare]

    type return =
      { id : int
      ; data : Customer.t
      ; created : string
      ; modified : string
      }
    [@@deriving yojson, compare]

    include Create (struct
      type a = provide
      type b = return

      let name = "api/customers"
      let select = [ "id"; "data"; "modified"; "created" ]
      let provide = provide_to_yojson
      let return = parse ~f:return_of_yojson
    end)
  end

  module Bookings = struct
    type provide =
      { customer : int
      ; data : Booking.t
      ; created : string
      ; modified : string
      }
    [@@deriving yojson, compare]

    type return =
      { id : int
      ; customer : int
      ; data : Booking.t
      ; created : string
      ; modified : string
      }
    [@@deriving yojson, compare]

    include Create (struct
      type a = provide
      type b = return

      let name = "api/bookings"
      let select = [ "id"; "customer"; "data"; "created"; "modified" ]
      let provide = provide_to_yojson
      let return = parse ~f:return_of_yojson
    end)
  end
end

include Schema
