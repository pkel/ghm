open Core_kernel

type verb =
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE

let string_of_verb = function
  | GET -> "GET"
  | POST -> "POST"
  | PUT -> "PUT"
  | PATCH -> "PATCH"
  | DELETE -> "DELETE"
;;

type ('a, 'b) t =
  { body : 'a -> string
  ; resp : string Lazy.t -> 'b Or_error.t
  ; url : string
  ; verb : verb
  ; params : string option String.Map.t
  ; headers : string String.Map.t
  }

let create ~url =
  { verb = GET
  ; resp = (fun _ -> Ok ())
  ; body = (fun () -> "")
  ; url
  ; params = String.Map.empty
  ; headers = String.Map.empty
  }
;;

let conv_resp ~f t = { t with resp = Fn.compose (Or_error.bind ~f) t.resp }
let map_resp ~f t = { t with resp = Fn.compose (Or_error.map ~f) t.resp }
let map_body ~f t = { t with body = Fn.compose t.body f }
let verb verb t = { t with verb }
let header ~key ~value t = { t with headers = String.Map.set ~key ~data:value t.headers }

let param ~key ?value t =
  let data = Option.map ~f:Browser.Misc.encode_uri_component value in
  let key = Browser.Misc.encode_uri_component key in
  { t with params = String.Map.set ~key ~data t.params }
;;

let want_text t =
  { t with
    resp =
      (function
      | (lazy s) -> Ok s)
  }
;;

let want_json t =
  let t' = header ~key:"accept" ~value:"application/json" t in
  { t' with
    resp =
      (function
      | (lazy str) ->
        (try Ok (Yojson.Safe.from_string str) with
        | e -> Error (Error.of_exn e)))
  }
;;

let give_json t =
  let t = header ~key:"Content-Type" ~value:"application/json" t in
  { t with body = Yojson.Safe.to_string }
;;

let give_text t =
  let t = header ~key:"Content-Type" ~value:"text/plain" t in
  { t with body = Fn.id }
;;

let bearer ~token =
  let key = "Authorization"
  and value = "Bearer " ^ token in
  header ~key ~value
;;

module XHR = struct
  open Browser.XHR

  let send ~body ~handler t =
    let xhr = create () in
    let url_with_params =
      String.Map.to_alist t.params
      |> List.map ~f:(fun (k, v_opt) ->
             match v_opt with
             | None -> k
             | Some v -> sprintf "%s=%s" k v)
      |> function
      | [] -> t.url
      | l ->
        let p = String.concat ~sep:"&" l in
        sprintf "%s?%s" (Browser.Misc.encode_uri t.url) p
    in
    open_ xhr (string_of_verb t.verb) url_with_params;
    String.Map.iteri t.headers ~f:(fun ~key ~data -> set_request_header xhr key data);
    set_onreadystatechange xhr (fun () ->
        match ready_state xhr with
        | Done ->
          let status = status xhr in
          if 200 <= status && status < 300
          then t.resp (lazy (response_text xhr)) |> handler
          else (
            let msg = status_text xhr in
            Or_error.errorf "XHR failed: %i %s" status msg |> handler)
        | _ -> ());
    send xhr (t.body body)
  ;;

  let send' ~handler t = send ~body:() ~handler t

  module Deferred = struct
    open Async_kernel

    let send ~body t =
      Deferred.create (fun cell ->
          let handler = Ivar.fill cell in
          send ~body ~handler t)
    ;;

    let send' t = send ~body:() t
  end
end
