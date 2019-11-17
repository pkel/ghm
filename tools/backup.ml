open Core_kernel
open Lwt
open Cohttp_lwt_unix

include struct
  [@@@warning "-39"]

  type args =
    { base : string [@default "http://localhost:8080/"] [@aka [ "b" ]]
    ; destination : string [@default "backup.json"] [@aka [ "d" ]]
    ; username : string [@aka [ "u" ]] [@env "GHM_USER"]
    ; password : string [@aka [ "p" ]] [@env "GHM_PASSWORD"]
    }
  [@@deriving cmdliner]
end

let main_lwt args =
  Pg.connect ~base_url:args.base ~username:args.username ~password:args.password
  >>= function
  | Error e -> Lwt.fail_with (Error.to_string_hum e)
  | Ok c ->
    Pg.(read Customers.t |> send' ~c)
    >>= (function
          | resp, Error e ->
            Format.eprintf "%a\n%a%!" Response.pp_hum resp Error.pp e;
            Lwt.fail_with "could not retrieve customers"
          | _, Ok customers -> Lwt.return customers)
    >|= List.map ~f:Pg.Customers.return_to_yojson
    >|= (fun l -> `List l)
    >|= Yojson.Safe.to_string
    >>= fun string ->
    Lwt_io.(open_file ~mode:Output args.destination)
    >>= fun out -> Lwt_io.write out string
;;

let main args = Lwt_main.run (main_lwt args)
let () = Cmdliner.Term.(exit @@ eval (const main $ args_cmdliner_term (), info "backup"))
