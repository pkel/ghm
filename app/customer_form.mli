open Incr_dom
open Base

module Model : sig
  type t [@@deriving compare]

  val create : unit -> t
end

module Action : sig
  type t [@@deriving sexp_of]

  val navchange : Nav.customer -> t
end

val create 
  :  back_href:string
  -> inject:(Action.t -> Vdom.Event.t)
  -> Model.t Incr.t
  -> (Action.t, Model.t, unit) Component.t Incr.t
