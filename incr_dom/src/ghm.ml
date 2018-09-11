open Core_kernel
open Async_kernel
open Incr_dom

module Model = struct
  type t = { counter : int } [@@deriving fields, compare]

  let cutoff t1 t2 = compare t1 t2 = 0
end

let initial_model = { Model.counter = 0 }

module Action = struct
  type t = Increment [@@deriving sexp_of]

  let should_log _t = true
end

module State = struct
  type t = unit
end

let apply_action (action: Action.t) (model: Model.t) _state : Model.t =
  match action with
  | Increment -> { counter = model.counter + 1 }

let update_visibility x = x

let view model ~inject:_ =
  let open Incr.Let_syntax in
  let%map counter =
    let%map counter = model >>| Model.counter in
    Vdom.Node.div [] [ Vdom.Node.text (Int.to_string counter)
                     ; Vdom.Node.text "hoi" ]
  in
  Vdom.Node.body [] [ counter ]

let on_startup ~schedule _model =
  every (Time_ns.Span.of_sec 1.) (fun () -> schedule Action.Increment);
  Deferred.unit

let on_display ~old:_ _model _state = ()
