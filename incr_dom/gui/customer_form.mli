open Ghm
open Incr_dom

module Model : sig
  type t [@@deriving compare]
  val empty: unit -> t
  val load: Customer.t -> t
end

module Action : sig
  type t [@@deriving sexp_of]
end

val create:
  save:(Customer.t -> Vdom.Event.t) ->
  back:Vdom.Event.t ->
  inject:(Action.t -> Vdom.Event.t) ->
  Model.t Incr.t ->
  (Action.t, Model.t, unit) Component.t Incr.t
