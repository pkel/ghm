open Core_kernel

type verb = GET | POST | PUT | PATCH | DELETE

let verb = function
  | GET -> "GET"
  | POST -> "POST"
  | PUT -> "PUT"
  | PATCH -> "PATCH"
  | DELETE -> "DELETE"

(** TODO: make this pure *)
type ('a, 'b) t =
  { body: 'a -> string
  ; resp: string -> 'b Or_error.t
  ; xhr: Browser.XHR.t
  ; url: string
  (* for error *) }

open Browser.XHR

let create ~v ~url =
  let xhr = create () in
  open_ xhr (verb v) url ;
  {xhr; resp= (fun s -> Ok s); body= (fun () -> ""); url}

let conv_resp ~f t = {t with resp= Fn.compose (Or_error.bind ~f) t.resp}

let map_resp ~f t = {t with resp= Fn.compose (Or_error.map ~f) t.resp}

let map_body ~f t = {t with body= Fn.compose t.body f}

let want_json t =
  set_request_header t.xhr "accept" "application/json" ;
  conv_resp t ~f:(fun str ->
      try Ok (Yojson.Safe.from_string str) with e -> Error (Error.of_exn e) )

let give_json t =
  set_request_header t.xhr "Content-Type" "application/json" ;
  {t with body= Yojson.Safe.to_string}

let give_text t =
  set_request_header t.xhr "Content-Type" "application/json" ;
  {t with body= Fn.id}

let prefer str t =
  set_request_header t.xhr "Prefer" str ;
  t

let send' ~handler t =
  set_onreadystatechange t.xhr (fun () ->
      match ready_state t.xhr with
      | Done ->
          let status = status t.xhr in
          if 200 <= status && status < 300 then
            t.resp (response_text t.xhr) |> handler
          else
            Or_error.errorf "request on %s failed with %i" t.url status
            |> handler
      | _ -> () ) ;
  send t.xhr (t.body ())

let send ~body ~handler t =
  set_onreadystatechange t.xhr (fun () ->
      match ready_state t.xhr with
      | Done ->
          let status = status t.xhr in
          if 200 <= status && status < 300 then
            t.resp (response_text t.xhr) |> handler
          else Or_error.errorf "XHR failed with %i" status |> handler
      | _ -> () ) ;
  send t.xhr (t.body body)
