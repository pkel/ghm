open Ghm
open Core_kernel
open Incr_dom

module Model : sig
  type t [@@deriving compare]
  val create: unit -> t
end

module Action : sig
  type t [@@deriving sexp_of]
end

val create:
  model:Model.t Incr.t ->
  old_model:(Model.t Incr.t) ->
  inject:(Action.t -> Vdom.Event.t) ->
  select:(int -> Vdom.Event.t) ->
  Customer.t Int.Map.t Incr.t ->
  (Action.t, Model.t, unit) Component.t Incr.t
