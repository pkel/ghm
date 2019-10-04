open Incr_dom
open Base
open Ghm

module Model : sig
  type t [@@deriving compare]

  val create : unit -> t
end

module Action : sig
  type t [@@deriving sexp_of]

  val navchange : Nav.noi * Nav.customer -> t
end

val create
  :  inject:(Action.t -> Vdom.Event.t)
  -> Model.t Incr.t
  -> (Action.t, Model.t, State.t, Menu.t * Customer.t) Component.with_extra Incr.t
