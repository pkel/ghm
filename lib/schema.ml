open Postgrest
open Setup
open Column

module Customers = struct
  type t

  let r : t resource = resource "customers"
  let id = int r "modified"
  let modified = date r "modified"
  let keyword = date r "keyword"
end

module Bookings = struct
  type t

  let r : t resource = resource "bookings"
  let id = int r "id"
  let modified = date r "modified"
  let arrival = date r "arrival"
  let departure = date r "departure"
end
