open Ghm
open Core_kernel
open Incr_dom

module RowId : module type of Int

module Model : sig
  type t [@@deriving compare]

  val create : unit -> t

  module Row : sig
    type t [@@deriving compare]

    val of_customer : id:int -> Customer.t -> t
    (** [id] is used with the select argument of {!create} *)
  end
end

module Action : sig
  type t [@@deriving sexp_of]
end

val create :
     model:Model.t Incr.t
  -> old_model:Model.t Incr.t
  -> inject:(Action.t -> Vdom.Event.t)
  -> select:(int -> Vdom.Event.t)
  -> Model.Row.t RowId.Map.t Incr.t
  -> (Action.t, Model.t, unit) Component.t Incr.t
