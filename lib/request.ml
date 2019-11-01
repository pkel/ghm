open Base

module Url : sig
  type url
  type param

  val param : key:string -> value:string option -> param
  val url : string -> param list -> url
end = struct
  type url = string
  type param = string * string option

  let param ~key ~value = key, value

  let url url = function
    | [] -> url
    | params ->
      url
      ^ "?"
      ^ String.concat
          ~sep:"&"
          (List.map params ~f:(fun (key, value) ->
               match value with
               | Some value -> key ^ "=" ^ value
               | None -> key))
  ;;
end

type verb =
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE

module type REQUEST = sig
  module Url = Url
  open Url

  (** A request with body ['a] and response type ['b]. *)
  type ('a, 'b) t

  type body

  val create : verb -> url -> (unit, unit) t
  val header : key:string -> value:string -> ('a, 'b) t -> ('a, 'b) t
  val give : content_type:string -> f:('a -> body) -> (unit, 'b) t -> ('a, 'b) t
  val want : accept:string -> f:(body -> 'b) -> ('a, unit) t -> ('a, 'b) t
  val map : f:('c -> 'a) -> ('a, 'b) t -> ('c, 'b) t
  val conv_resp : f:('b -> 'c Or_error.t) -> ('a, 'b) t -> ('a, 'c) t
  val map_resp : f:('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
end
