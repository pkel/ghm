open Core_kernel
open Lwt
open Cohttp_lwt_unix

include struct
  [@@@warning "-39"]

  type args =
    { base : string [@default "http://localhost:8080/"] [@aka [ "b" ]]
    ; file : string [@default "backup.json"] [@aka [ "f" ]]
    ; username : string [@aka [ "u" ]] [@env "GHM_USER"]
    ; password : string [@aka [ "p" ]] [@env "GHM_PASSWORD"]
    ; restore : bool [@aka [ "r" ]] [@default false]
    }
  [@@deriving cmdliner]
end

type customer =
  { id : int
  ; data : Ghm.Customer.t
  ; bookings : Ghm.Booking.t list
  }
[@@deriving yojson { strict = false }]

type t = { customers : customer list } [@@deriving yojson { strict = false }]

let backup c args =
  Pg.(read Customers.t |> send' ~c)
  >>= (function
        | resp, Error e ->
          Format.eprintf "%a\n%a\n%!" Response.pp_hum resp Error.pp e;
          Lwt.fail_with "could not retrieve customers"
        | _, Ok customers -> Lwt.return customers)
  >>= Lwt_list.map_p (fun { Pg.Customers.id; data; _ } ->
          Pg.(read ~filter:Int.(Bookings.customer = id) Bookings.t |> send' ~c)
          >>= function
          | resp, Error e ->
            Format.eprintf "%a\n%a\n%!" Response.pp_hum resp Error.pp e;
            Lwt.fail_with (sprintf "could not retrieve bookings for customer %i" id)
          | _, Ok bookings ->
            Lwt.return { id; data; bookings = List.map ~f:(fun x -> x.data) bookings })
  >|= (fun customers -> { customers })
  >|= to_yojson
  >|= Yojson.Safe.to_string
  >>= fun string ->
  Lwt_io.(open_file ~mode:Output args.file) >>= fun out -> Lwt_io.write out string
;;

let restore c args =
  Lwt_io.(open_file ~mode:Input args.file)
  >>= Lwt_io.read
  >|= Yojson.Safe.from_string
  >|= of_yojson
  >|= (function
        | Ok backup -> backup.customers
        | Error e ->
          Format.eprintf "%s\n%!" e;
          failwith "could not read backup")
  >|= List.fold ~init:Int.Map.empty ~f:(fun acc data ->
          match Int.Map.add acc ~data ~key:data.id with
          | `Ok a -> a
          | `Duplicate -> failwith "duplicate customer ids in backup")
  >>= fun backup_map ->
  let body =
    let data = Int.Map.data backup_map in
    List.map data ~f:(fun { id; data; _ } -> { data with _import_key = Some id })
  in
  Pg.(create_m Customers.t |> send ~c ~body)
  >|= (function
        | resp, Error e ->
          Format.eprintf "%a\n%a\n%!" Response.pp_hum resp Error.pp e;
          failwith "could not create customers"
        | _, Ok customers ->
          List.fold customers ~init:Int.Map.empty ~f:(fun acc data ->
              let key =
                match data.data._import_key with
                | None -> failwith "import key missing on remote data"
                | Some a -> a
              in
              Int.Map.set acc ~data:data.id ~key))
  >>= fun id_map ->
  let bookings =
    Int.Map.fold2 id_map backup_map ~init:[] ~f:(fun ~key:_ ~data acc ->
        match data with
        | `Both (id, local) ->
          let bookings =
            List.map local.bookings ~f:(fun data -> { Pg.Bookings.customer = id; data })
          in
          bookings @ acc
        | _ -> failwith "customer id mismatch")
  in
  Pg.(create_m Bookings.t |> send ~c ~body:bookings)
  >|= function
  | resp, Error e ->
    Format.eprintf "%a\n%a\n%!" Response.pp_hum resp Error.pp e;
    failwith "could not create bookings"
  | _, Ok _ -> ()
;;

let main args =
  Pg.connect ~base_url:args.base ~username:args.username ~password:args.password
  >>= function
  | Error e -> Lwt.fail_with (Error.to_string_hum e)
  | Ok c -> if args.restore then restore c args else backup c args
;;

let () =
  let main args = Lwt_main.run (main args) in
  Cmdliner.Term.(exit @@ eval (const main $ args_cmdliner_term (), info "backup"))
;;
