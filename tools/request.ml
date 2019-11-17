open Core_kernel
open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix
open Postgrest.Request
open Lwt.Infix

type body = string

type ('a, 'b) t =
  { body : 'a -> body
  ; resp : body -> 'b Or_error.t
  ; url : Url.t
  ; verb : verb
  ; headers : Header.t
  }

let create verb url =
  { verb; resp = (fun _ -> Ok ()); body = (fun () -> ""); url; headers = Header.init () }
;;

let conv_resp ~f t = { t with resp = Fn.compose (Or_error.bind ~f) t.resp }
let map_resp ~f t = { t with resp = Fn.compose (Or_error.map ~f) t.resp }
let map ~f t = { t with body = Fn.compose t.body f }
let header ~key ~value t = { t with headers = Header.add t.headers key value }

let want ~accept t =
  { t with resp = (fun s -> Ok s) } |> header ~key:"accept" ~value:accept
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

type connection =
  { mutable token : string
  ; base_url : string
  }

let check_resp (resp, body) =
  let code = resp |> Response.status in
  Body.to_string body
  >|= function
  | body ->
    if Code.code_of_status code >= 400
    then resp, Or_error.error_string body
    else resp, Ok body
;;

let send ~c ~body t =
  let t =
    let key = "Authorization"
    and value = "Bearer " ^ c.token in
    header ~key ~value t
  in
  let body = t.body body |> Body.of_string
  and url = Uri.of_string (c.base_url ^ Url.to_string t.url)
  and headers = t.headers in
  (match t.verb with
  | GET -> Client.get ~headers url
  | POST -> Client.post ~body ~headers url
  | PATCH -> Client.patch ~body ~headers url
  | DELETE -> Client.delete ~body ~headers url
  | PUT -> Client.put ~body ~headers url)
  >>= check_resp
  >|= fun (resp, body) -> resp, Or_error.bind ~f:t.resp body
;;

let send' ~c t = send ~c ~body:() t

let connect ~base_url ~username ~password =
  let uri = base_url ^ "/"
  and headers = Header.of_list [ "Content-Type", "application/x-www-form-urlencoded" ]
  and body =
    sprintf "action=token&username=%s&password=%s" username password |> Body.of_string
  in
  Client.post ~headers ~body (Uri.of_string uri)
  >>= check_resp
  >|= fun (_, token) -> Or_error.map ~f:(fun token -> { token; base_url }) token
;;
