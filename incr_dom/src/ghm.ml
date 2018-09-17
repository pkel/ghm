open Core_kernel
open Async_kernel
open Incr_dom

module Model = struct

  type customer =
    { t : Customer.t
    ; bookings : Booking.t Int.Map.t
    }

  and t =
    { counter : int
    ; store : customer Int.Map.t
    }
  [@@deriving fields, compare]

  let cutoff t1 t2 = compare t1 t2 = 0
end

let initial_model =
  { Model.counter = 0
  ; store = Int.Map.empty }

module Action = struct
  type t = Increment [@@deriving sexp_of]

  let should_log _t = true
end

module State = struct
  type t = unit
end

let apply_action (a: Action.t) (m: Model.t) _state ~schedule_action:_ =
  match a with
  | Increment -> { m with counter = m.counter + 1 }

let update_visibility x = x

let view model ~inject:_ =
  let open Incr.Let_syntax in
  let%map counter =
    let%map counter = model >>| Model.counter in
    Vdom.Node.div [] [ Vdom.Node.text (Int.to_string counter)
                     ; Vdom.Node.text "hoi" ]
  in
  Vdom.Node.body [] [ counter ]

let on_startup ~schedule_action _model =
  every (Time_ns.Span.of_sec 1.) (fun () -> schedule_action Action.Increment);
  Deferred.unit

let on_display ~old_model:_ _model _state ~schedule_action:_ = ()
