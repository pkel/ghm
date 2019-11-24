open Base
open Incr_dom

type 'a inject = 'a -> Vdom.Event.t

module type EDITABLE = sig
  type env
  type t

  module Model : sig
    type t [@@deriving compare]
  end

  val init : t -> Model.t
  val eval : Model.t -> t Or_error.t

  module Action : sig
    type t [@@deriving sexp_of]
  end

  val create
    :  env:env
    -> inject:Action.t inject
    -> Model.t Incr.t
    -> (Action.t, Model.t, State.t) Component.t Incr.t
end

module type FORM_COMPONENT = sig
  type env
  type edit

  module Model : sig
    type t [@@deriving compare]

    val load : edit -> t
    val read : t -> edit
  end

  module Action : sig
    type t [@@deriving sexp_of]
  end

  val create
    :  env:env
    -> inject:Action.t inject
    -> Model.t Incr.t
    -> (Action.t, Model.t, State.t, Menu.t) Component.with_extra Incr.t
end
