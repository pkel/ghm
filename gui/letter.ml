open Core_kernel
open Ghm
open Incr_dom
open Incr_dom_widgets
open Incr.Let_syntax

module Letter = struct
  type t =
    { address : string
    ; sender : string
    ; subject : string
    ; body : string }
  [@@deriving fields]
end

let form_descr =
  let open Form.Description in
  let open Of_record in
  build_for_record
    (Letter.Fields.make_creator
       ~address:(field string)
       ~sender:(field string)
       ~subject:(field string)
       ~body:(field string))
;;

let letter_form = Form.create ~name:"letter form" form_descr

module Model = struct
  type t = {form : Form.State.t} [@@deriving compare, fields]

  let create () = {form = Form.State.create letter_form}
end

module Action = struct
  type t = Unit [@@deriving sexp, variants]
end

module State = struct
  type t = unit
end

let apply_action
    (model : Model.t) (action : Action.t) (_state : State.t) ~schedule_action : Model.t =
  let _ = schedule_action in
  match action with Unit -> model
;;

open Vdom

let view_letter state ids =
  let address, (sender, (subject, (body, ()))) = ids in
  [ Form.Input.textarea state address []
  ; Form.Input.textarea state sender []
  ; Form.Input.textarea state subject []
  ; Form.Input.textarea state body [] ]
;;

let view (model : Model.t Incr.t) ~inject =
  let open Vdom in
  let%map form = model >>| Model.form in
  let ids = Form.State.field_ids form letter_form in
  Node.create "form" [] (view_letter form ids)
;;

let create ~(inject : Action.t -> Vdom.Event.t) (model : Model.t Incr.t) =
  let%map model = model
  and view = view ~inject model in
  let apply_action = apply_action model in
  Component.create ~apply_action model view
;;
