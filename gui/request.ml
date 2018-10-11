
type response_handler = (string, int) result -> unit
type verb =
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE

let verb = function
  | GET -> "GET"
  | POST -> "POST"
  | PUT -> "PUT"
  | PATCH -> "PATCH"
  | DELETE -> "DELETE"

let send ~v ?(body="") return url =
  let open Browser.XHR in
  let xhr = create () in
  open_ xhr (verb v) url;
  set_onreadystatechange xhr (fun () ->
      match ready_state xhr with
      | Done ->
        let status = status xhr in
        if 200 <= status && status < 300
        then Ok (response_text xhr) |> return
        else Error status |> return
      | _ -> ()
    );
  send xhr body
