open Async_js
open Incr_dom_widgets
open Core_kernel

let error msg =
  let msg = "ERROR: " ^ msg in
  log_s [%message msg]

let form state =
  List.iter (Form.State.errors state) ~f:(fun e ->
      let msg = Format.asprintf "FORM: %s" (Error.to_string_hum e) in
      log_s [%message msg])
