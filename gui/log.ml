open Incr_dom_widgets
open Core_kernel
open Browser
open Console

let log_string s = log console (Ojs.string_to_js s)

let error_str msg =
  let msg = "ERROR: " ^ msg in
  log_string msg

let error e =
  let msg = Error.to_string_hum e in
  error_str msg

let form state =
  List.iter (Form.State.errors state) ~f:(fun e ->
      let msg = Format.asprintf "FORM: %s" (Error.to_string_hum e) in
      log_string msg )
