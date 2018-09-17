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
  ; store = Int.Map.empty
  }

module Action = struct
  type t = Increment [@@deriving sexp_of]
end

module State = struct
  type t = unit
end

let create model ~old_model:_ ~inject:_ =
  let open Incr.Let_syntax in
  let%map apply_action =
    let%map model = model
    and counter = model >>| Model.counter in
    fun (Increment : Action.t) _ ~schedule_action:_ ->
      { model with Model.counter = counter + 1 }
  and view =
    let%map counter =
      let%map counter = model >>| Model.counter in
      Vdom.Node.div [] [ Vdom.Node.text (Int.to_string counter)
                       ; Vdom.Node.text " - hoi" ]
    in
    Vdom.Node.body [] [ counter ]
  and model = model in
  (* Note that we don't include [on_display] or [update_visibility], since
     these are optional arguments *)
  Component.create ~apply_action model view

let on_startup ~schedule_action _model =
  every (Time_ns.Span.of_sec 1.) (fun () -> schedule_action Action.Increment);
  Deferred.unit
