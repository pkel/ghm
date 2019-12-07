open Incr_dom
open Base

module Model : sig
  type t [@@deriving compare]

  val create : unit -> t
end

module Action : sig
  type t [@@deriving sexp_of]

  val refresh : t
end

val create
  :  inject:(Action.t -> Vdom.Event.t)
  -> Model.t Incr.t
  -> (Action.t, Model.t, State.t) Component.t Incr.t
