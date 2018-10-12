type response = (Yojson.Safe.json, int) result

type 'a handler = 'a -> unit

type verb = GET | POST | PUT | PATCH | DELETE

let verb = function
  | GET -> "GET"
  | POST -> "POST"
  | PUT -> "PUT"
  | PATCH -> "PATCH"
  | DELETE -> "DELETE"

let send ?body ?prefer ~v return url =
  let open Browser.XHR in
  let xhr = create () in
  open_ xhr (verb v) url ;
  set_request_header xhr "accept" "application/json" ;
  (match prefer with None -> () | Some p -> set_request_header xhr "Prefer" p) ;
  set_onreadystatechange xhr (fun () ->
      match ready_state xhr with
      | Done ->
          let status = status xhr in
          if 200 <= status && status < 300 then
            Ok (Yojson.Safe.from_string (response_text xhr)) |> return
          else Error status |> return
      | _ -> () ) ;
  let body =
    match body with
    | None -> ""
    | Some j ->
        set_request_header xhr "Content-Type" "application/json" ;
        Yojson.Safe.to_string j
  in
  send xhr body
