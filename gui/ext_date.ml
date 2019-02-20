open Core_kernel
include Core_kernel.Date

(* TODO: move date_yojson here *)

let of_browser_date x : t Or_error.t =
  let open Browser.Date in
  let y = get_full_year x
  and m = get_month x + 1
  and d = get_date x in
  Or_error.try_with (fun () -> create_exn ~y ~m:(Month.of_int_exn m) ~d)
;;

let today () =
  match of_browser_date Browser.Date.(now ()) with
  | Ok date -> date
  | Error e ->
    Log.error e;
    Date.of_string "2019-01-01"
;;
