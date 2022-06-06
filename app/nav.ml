module Navigator : sig
  type path = string list
  type 'a t

  val create : of_path:(path -> 'a) -> to_path:('a -> path) -> 'a t

  (** Listen for hashchange events. Triggers on {!set} and {!navigate} events.
      *)
  val listen : handler:('a -> unit) -> 'a t -> unit

  (** Access the current location. Useful during initialization. *)
  val get : 'a t -> 'a

  (** Set the location. Triggers the handler provided to {!listen}. Useful
      during apply_action. *)
  val set : 'a t -> 'a -> unit

  (** Wrapper on {!set} used for onClick properties of {!Vdom} elements. *)
  val navigate : 'a t -> 'a -> 'b -> Incr_dom.Vdom.Event.t

  (** URL for use in HTML anchors *)
  val href : 'a t -> 'a -> string
end = struct
  open Base

  type path = string list

  type 'a t =
    { of_path : path -> 'a
    ; to_path : 'a -> path
    }

  let create ~of_path ~to_path = { of_path; to_path }

  let path_of_string s =
    let drop c = Caml.List.mem c [ '/'; '#' ] in
    String.split ~on:'/' (String.strip ~drop s)
  ;;

  let string_of_path p = "/" ^ String.concat ~sep:"/" p
  let string t a = t.to_path a |> string_of_path
  let get t = Browser.Location.get_hash () |> path_of_string |> t.of_path
  let set t a = t.to_path a |> string_of_path |> Browser.Location.set_hash

  let navigate t a _evt =
    set t a;
    Incr_dom.Vdom.Event.Ignore
  ;;

  let href t a = "#" ^ string t a

  let listen ~handler t =
    Browser.Window.add_event_listener
      Browser.window
      "hashchange"
      (fun _ -> handler (get t))
      false
  ;;
end

open Base

type noi =
  | New
  | Id of int
[@@deriving compare, sexp_of]

let noi_to_string = function
  | New -> "new"
  | Id i -> Int.to_string i
;;

let noi_of_string_opt = function
  | "new" -> Some New
  | s ->
    (match Caml.int_of_string_opt s with
    | Some i -> Some (Id i)
    | None -> None)
;;

type customer =
  | CData
  | Booking of noi
[@@deriving compare, sexp_of]

let customer_of_path = function
  | "booking" :: i :: _ ->
    (match noi_of_string_opt i with
    | Some i -> Booking i
    | None -> CData)
  | _ -> CData
;;

let customer_to_path = function
  | CData -> []
  | Booking i -> [ "booking"; noi_to_string i ]
;;

type main =
  | Overview
  | Statistics
  | Search
  | Customer of (noi * customer)
[@@deriving compare, sexp_of]

let default = Overview

let of_path = function
  | "customer" :: i :: tl ->
    (match noi_of_string_opt i with
    | Some i -> Customer (i, customer_of_path tl)
    | None -> default)
  | [ "statistics" ] -> Statistics
  | [ "search" ] -> Search
  | _ -> default
;;

let to_path = function
  | Overview -> []
  | Statistics -> [ "statistics" ]
  | Search -> [ "search" ]
  | Customer (i, c) -> "customer" :: noi_to_string i :: customer_to_path c
;;

open Navigator

let navigator = create ~of_path ~to_path
let listen = listen navigator
let get () = get navigator
let set = set navigator
let nav _ev = navigate navigator _ev
let href = href navigator
