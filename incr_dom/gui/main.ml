open Core_kernel
open Ghm
open Async_kernel
open Incr_dom

module Model = struct

  type t =
    { counter : int
    ; db : Storage.t
    }
  [@@deriving compare, fields]

  let cutoff t1 t2 = compare t1 t2 = 0
end

let initial_model =
  { Model.counter = 0
  ; db = Storage.empty
  }

module Action = struct
  type t =
    | Increment
    | LoadDump of string
  [@@deriving sexp_of]
end

module State = struct
  type t = unit
end

let create model ~old_model:_ ~inject:_ =
  let open Incr.Let_syntax in
  let%map apply_action =
    let%map model = model
    and counter = model >>| Model.counter in
    fun (a : Action.t) _ ~schedule_action:_ ->
      match a with
      | Increment ->
        { model with Model.counter = counter + 1 }
      | LoadDump s ->
        { model with Model.db = Storage.of_string s }
  and view =
    let%map counter =
      let%map counter = model >>| Model.counter
      and size = model >>| Model.db >>| Storage.size in
      Vdom.Node.div [] [ Vdom.Node.text (Int.to_string counter)
                       ; Vdom.Node.text " - "
                       ; Vdom.Node.text (Int.to_string size)]
    in
    Vdom.Node.body [] [ counter ]
  and model = model in
  Component.create ~apply_action model view

let on_startup ~schedule_action _model =
  every (Time_ns.Span.of_sec 1.) (fun () -> schedule_action Action.Increment);
  Async_js.Http.get "data.sexp" >>| function
  | Ok s -> schedule_action (LoadDump s)
  | _ -> ()
