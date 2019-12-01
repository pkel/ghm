open Incr_dom
open Base
open Ghm

type env =
  { nav : Nav.noi * Nav.booking Incr.t
  ; rel : Nav.noi * Nav.booking -> Nav.main
  ; customer : Customer.t Incr.t
  ; customer_id : Nav.noi Incr.t
  }

module Model : sig
  type t [@@deriving compare]

  val create : unit -> t
end

module Action : sig
  type t [@@deriving sexp_of]

  val navchange : Nav.noi * Nav.booking -> t
end

val create
  :  inject:(Action.t -> Vdom.Event.t)
  -> env:env Incr.t
  -> Model.t Incr.t
  -> (Action.t, Model.t, State.t, Menu.t) Component.with_extra Incr.t
