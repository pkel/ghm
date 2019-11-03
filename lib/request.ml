open Base

module Url : sig
  type t
  type param

  val param : string -> string -> param
  val param' : string -> param
  val url : string -> param list -> t
  val to_string : t -> string
end = struct
  type t = string
  type param = string * string option

  let param key value = key, Some value
  let param' key = key, None

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

  let to_string s = s
end

type verb =
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE

module type REQUEST = sig
  (** A request with body ['a] and response type ['b]. *)
  type ('a, 'b) t

  type body

  val create : verb -> Url.t -> (unit, unit) t
  val header : key:string -> value:string -> ('a, 'b) t -> ('a, 'b) t
  val give : content_type:string -> (unit, 'b) t -> (body, 'b) t
  val want : accept:string -> ('a, unit) t -> ('a, body) t
  val map : f:('c -> 'a) -> ('a, 'b) t -> ('c, 'b) t
  val conv_resp : f:('b -> 'c Or_error.t) -> ('a, 'b) t -> ('a, 'c) t
  val map_resp : f:('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
end
