type response_handler = (string, int) result -> unit

type verb =
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE

val send: v: verb -> ?body: string -> response_handler -> string -> unit
