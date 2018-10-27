type customer =
  | New
  | Id of int

and t =
  | Overview
  | Customer of customer
[@@deriving compare]

val href_of : t -> string

(** Listen for hashchange events. Triggers on {!set} and {!navigate} events. *)
val listen : (t option -> unit) -> unit

(** Access the current location. Useful during initialization. *)
val get : unit -> t option

(** Set the location. Triggers the handler provided to {!listen}. Useful during
    apply_action. *)
val set : t -> unit

(** Event handler wrapper for {!set}. Useful for onclick properties of Vdom
    elements. *)
val navigate : t -> 'a -> Incr_dom.Vdom.Event.t
