open Base

type customer =
  | New
  | Id of int

and t =
  | Overview
  | Customer of customer
[@@deriving compare]

let customer_of_path = function
  | ["new"] -> Some New
  | [s] -> Option.map ~f:(fun i -> Id i) (Caml.int_of_string_opt s)
  | _ -> None
;;

let customer_to_path = function New -> ["new"] | Id i -> [Int.to_string i]

let of_path = function
  | ["overview"] -> Some Overview
  | "customer" :: tl -> Option.map ~f:(fun x -> Customer x) (customer_of_path tl)
  | _ -> None
;;

let path_of = function
  | Overview -> ["overview"]
  | Customer x -> "customer" :: customer_to_path x
;;

let path_of_string s =
  let drop c = Caml.List.mem c ['/'; '#'] in
  String.split ~on:'/' (String.strip ~drop s)
;;

let string_of_path p = "/" ^ String.concat ~sep:"/" p
let string_of = Fn.compose string_of_path path_of
let of_string = Fn.compose of_path path_of_string
let href_of t = "#" ^ string_of t
let get () = of_string (Browser.Location.get_hash ())
let set t = Browser.Location.set_hash (string_of t)

let navigate t _evt =
  set t;
  Incr_dom.Vdom.Event.Ignore
;;

let listen on_change =
  Browser.Window.add_event_listener
    Browser.window
    "hashchange"
    (fun _ -> on_change (get ()))
    false
;;
