type response = (Yojson.Safe.json, int) result
type 'a handler = 'a -> unit

type verb =
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE

val send:
  ?body:Yojson.Safe.json ->
  ?prefer:string ->
  v: verb ->
  response handler -> string -> unit
