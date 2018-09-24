open Core_kernel
open Ghm
open Incr_dom
open Incr.Let_syntax

module Model = struct
  type t = unit
  [@@deriving compare]

  let create () = ()
end

module Action = struct
  type t = unit [@@deriving sexp]
end

let apply_action _action _state ~schedule_action:_ = ()

let view (customer : Customer.t option Incr.t) =
  let open Vdom in
  let open Node in
  let%map c = customer in
  match c with
  | None -> div []
              [ text "Dieser Kunde existiert noch nicht oder nicht mehr." ]
  | Some c -> div []
                [ text "Schlüssel: "
                ; text c.keyword ]

let create
    ~(inject:(Action.t -> Vdom.Event.t))
    ~(model:Model.t Incr.t)
    (c: Customer.t option Incr.t) =
  let _ = inject () in
  let%map model = model
  and view = view c in
  Component.create ~apply_action model view
