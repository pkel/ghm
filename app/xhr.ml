open Core_kernel
open Postgrest.Request

let string_of_verb = function
  | GET -> "GET"
  | POST -> "POST"
  | PUT -> "PUT"
  | PATCH -> "PATCH"
  | DELETE -> "DELETE"
;;

type body = string

type ('a, 'b) t =
  { body : 'a -> body
  ; resp : body Lazy.t -> 'b Or_error.t
  ; url : Url.t
  ; verb : verb
  ; headers : string String.Map.t
  }

let create verb url =
  { verb
  ; resp = (fun _ -> Ok ())
  ; body = (fun () -> "")
  ; url
  ; headers = String.Map.empty
  }
;;

let conv_resp ~f t = { t with resp = Fn.compose (Or_error.bind ~f) t.resp }
let map_resp ~f t = { t with resp = Fn.compose (Or_error.map ~f) t.resp }
let map ~f t = { t with body = Fn.compose t.body f }
let header ~key ~value t = { t with headers = String.Map.set ~key ~data:value t.headers }

let want ~accept t =
  { t with resp = (fun s -> Ok (force s)) } |> header ~key:"accept" ~value:accept
;;

let give ~content_type t =
  { t with body = (fun s -> s) } |> header ~key:"Content-Type" ~value:content_type
;;

let want_json ?(accept = "application/json") t =
  want ~accept t
  |> conv_resp ~f:(fun str ->
         try Ok (Yojson.Safe.from_string str) with
         | e -> Error (Error.of_exn e))
;;

let give_json ?(content_type = "application/json") t =
  give ~content_type t |> map ~f:Yojson.Safe.to_string
;;

let bearer ~token =
  let key = "Authorization"
  and value = "Bearer " ^ token in
  header ~key ~value
;;

type connection = { mutable token : string }

let finalize c = bearer ~token:c.token

let get_token =
  create GET (Url.url "" [ Url.param "action" "token" ]) |> want ~accept:"text/plain"
;;

open Browser.XHR

let send ?c ~body ~handler t =
  let t =
    match c with
    | Some c -> finalize c t
    | None -> t
  in
  let xhr = create () in
  let url_with_params =
    Url.to_string ~url_encode:Browser.Misc.encode_uri_component t.url
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

let send' ?c ~handler t = send ?c ~body:() ~handler t

open Async_kernel

let asend ?c ~body t =
  Deferred.create (fun cell ->
      let handler = Ivar.fill cell in
      send ?c ~body ~handler t)
;;

let asend' ?c t = asend ?c ~body:() t

let refresh c () =
  asend' get_token
  >>| function
  | Ok t -> c.token <- t
  (* TODO think about error case *)
  | Error _ -> ()
;;

let connect () =
  (* Token valid for 300s *)
  let delta = Time_ns.Span.of_sec 240. in
  asend' get_token
  >>| function
  | Ok token ->
    let c = { token } in
    every ~start:(after delta) delta (fun () -> don't_wait_for (refresh c ()));
    Some c
  | Error _ -> None
;;

let send ~c = send ~c
let send' ~c = send' ~c

module Deferred = struct
  let send ~c = asend ~c
  let send' ~c = asend' ~c
end
