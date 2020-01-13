open Incr_dom
open Base
open Ghm

type env =
  { customer : Customer.t Incr.t
  ; customer_id : Nav.noi Incr.t
  }

module Model : sig
  type t [@@deriving compare]

  val create : unit -> t
  val booking_id : t -> Nav.noi
end

module Action : sig
  type t [@@deriving sexp_of]

  val navchange : Nav.noi * Nav.booking -> t
end

val create
  :  env:env
  -> inject:(Action.t -> Vdom.Event.t)
  -> Model.t Incr.t
  -> (Action.t, Model.t, State.t, Menu.t * Period.t option) Component.with_extra Incr.t
