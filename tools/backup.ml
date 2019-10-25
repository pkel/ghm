open Core_kernel
open Lwt
open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix

include struct
  [@@@warning "-39"]

  type args =
    { base : string [@default "http://localhost"] [@aka [ "b" ]]
    ; destination : string [@default "backup.json"] [@aka [ "d" ]]
    ; username : string [@aka [ "u" ]] [@env "GHM_USER"]
    ; password : string [@aka [ "p" ]] [@env "GHM_PASSWORD"]
    }
  [@@deriving cmdliner]
end

let check_resp uri (resp, body) =
  let code = resp |> Response.status in
  if Code.code_of_status code > 400
  then
    Lwt.fail_with (sprintf "Request to %s returned %s" uri (Code.string_of_status code))
  else Body.to_string body
;;

let main_lwt args =
  let uri = args.base ^ "/"
  and headers = Header.of_list [ "Content-Type", "application/x-www-form-urlencoded" ]
  and body =
    sprintf "action=token&username=%s&password=%s" args.username args.password
    |> Body.of_string
  in
  Client.post ~headers ~body (Uri.of_string uri)
  >>= check_resp uri
  >>= fun token ->
  let uri = args.base ^ "/api/customers"
  and headers = Header.of_list [ "Authorization", "Bearer " ^ token ] in
  Client.get ~headers (Uri.of_string uri)
  >>= check_resp uri
  >>= fun body ->
  Lwt_io.(open_file ~mode:Output args.destination) >>= fun out -> Lwt_io.write out body
;;

let main args = Lwt_main.run (main_lwt args)
let () = Cmdliner.Term.(exit @@ eval (const main $ args_cmdliner_term (), info "backup"))
