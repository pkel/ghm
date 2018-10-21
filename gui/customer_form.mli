open Incr_dom
open Base

module Navigation : sig
  type t [@@deriving compare, sexp_of]

  val of_path : string list -> t option

  val to_path : t -> string list

  val id : int -> t

  val new_ : t
end

module Model : sig
  type t [@@deriving compare]

  val create : unit -> t
end

module Action : sig
  type t [@@deriving sexp_of]

  val navigate : Navigation.t -> t
end

val create :
     back_href:string
  -> inject:(Action.t -> Vdom.Event.t)
  -> Model.t Incr.t
  -> (Action.t, Model.t, unit) Component.t Incr.t
