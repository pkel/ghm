open Incr_dom

type 'a inject = 'a -> Vdom.Event.t

module type FORM_COMPONENT = sig
  type env
  type edit

  module Model : sig
    type t [@@deriving compare]

    val load : edit -> t
  end

  module Action : sig
    type t [@@deriving sexp_of]
  end

  val create
    :  env:env
    -> inject:Action.t inject
    -> Model.t Incr.t
    -> (Action.t, Model.t, State.t, edit) Component.with_extra Incr.t
end
