open Incr_dom

module type FORM_COMPONENT = sig
  type edit

  module Model : sig
    type t [@@deriving compare]

    val load : edit -> t
  end

  module Action : sig
    type t [@@deriving sexp_of]
  end

  val create
    :  inject:(Action.t -> Vdom.Event.t)
    -> Model.t Incr.t
    -> (Action.t, Model.t, State.t, edit) Component.with_extra Incr.t
end
