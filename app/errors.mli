open Base
open Incr_dom

module Model : sig
  type t [@@deriving compare]

  val empty : t
end

module Action : sig
  type t [@@deriving sexp_of]

  val log : State.error -> t
end

val create
  :  inject:(Action.t -> Vdom.Event.t)
  -> Model.t Incr.t
  -> (Action.t, Model.t, State.t) Component.t Incr.t
