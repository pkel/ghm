open Incr_dom_widgets
open Core_kernel
open Browser
open Console

let str s = log console (Ojs.string_to_js s)
let logf f = Printf.ksprintf str f

let error_str msg =
  let msg = "ERROR: " ^ msg in
  str msg
;;

let errorf f = Printf.ksprintf error_str f

let error e =
  let msg = Error.to_string_hum e in
  error_str msg
;;

let form state =
  List.iter (Form.State.errors state) ~f:(fun e ->
      let msg = "FORM: " ^ Error.to_string_hum e in
      error_str msg)
;;
