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

type t =
  { customers : Pg.Customers.return list
  ; bookings : Pg.Bookings.return list
  }
[@@deriving yojson { strict = false }]

let backup c args =
  Pg.(read Customers.t |> send' ~c)
  >>= (function
        | resp, Error e ->
          Format.eprintf "%a\n%a\n%!" Response.pp_hum resp Error.pp e;
          Lwt.fail_with "could not retrieve customers"
        | _, Ok customers -> Lwt.return customers)
  >>= fun customers ->
  Pg.(read Bookings.t |> send' ~c)
  >>= (function
        | resp, Error e ->
          Format.eprintf "%a\n%a\n%!" Response.pp_hum resp Error.pp e;
          Lwt.fail_with "could not retrieve customers"
        | _, Ok bookings -> Lwt.return bookings)
  >|= (fun bookings -> { customers; bookings })
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
        | Ok backup -> backup
        | Error e ->
          Format.eprintf "%s\n%!" e;
          failwith "could not read backup")
  >>= fun backup ->
  let customer_map =
    (* Customers indexed by old customer id *)
    List.fold backup.customers ~init:Int.Map.empty ~f:(fun acc data ->
        let open Pg.Customers in
        match Int.Map.add acc ~data ~key:data.id with
        | `Ok a -> a
        | `Duplicate -> failwith "duplicate customer ids in backup")
  in
  let body =
    let data = Int.Map.data customer_map in
    List.map data ~f:(fun { id; data; modified; created } ->
        let open Pg.Customers in
        let data = { data with _import_key = Some id } in
        { data; modified; created })
  in
  Pg.(create_m Customers.t |> send ~c ~body)
  >|= (function
        | resp, Error e ->
          Format.eprintf "%a\n%a\n%!" Response.pp_hum resp Error.pp e;
          failwith "could not create customers"
        | _, Ok customers -> customers)
  >>= fun customers ->
  let customer_id_map =
    (* New customer ids indexed by old customer id *)
    List.fold customers ~init:Int.Map.empty ~f:(fun acc data ->
        let key =
          match data.data._import_key with
          | None -> failwith "import key missing on remote data"
          | Some a -> a
        in
        Int.Map.set acc ~data:data.id ~key)
  in
  let booking_map =
    (* Bookings indexed by old customer id *)
    List.fold backup.bookings ~init:Int.Map.empty ~f:(fun acc data ->
        let open Pg.Bookings in
        Int.Map.add_multi acc ~data ~key:data.customer)
  in
  let bookings =
    Int.Map.fold2 customer_id_map booking_map ~init:[] ~f:(fun ~key:_ ~data acc ->
        match data with
        | `Both (customer, bookings) ->
          let bookings =
            List.map bookings ~f:(fun { data; modified; created; _ } ->
                let open Pg.Bookings in
                { data; modified; customer; created })
          in
          bookings @ acc
        | `Left _ -> acc
        | `Right _ -> failwith "customer id mismatch")
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
