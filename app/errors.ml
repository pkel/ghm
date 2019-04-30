open Base
open Incr_dom
open Incr.Let_syntax

module Model = struct
  type entry =
    { seen : bool
    ; error : State.error
    ; time : string }
  [@@deriving compare]

  type t =
    { lst : entry list
    ; visible : bool }
  [@@deriving compare]

  let empty = {visible = false; lst = []}
end

module Action = struct
  type t =
    | Log of State.error
    | Open
    | Close
  [@@deriving sexp_of]

  let log e = Log e
end

let eval_js id expr =
  let expr =
    Printf.sprintf
      {| try { %s } catch (e) {
          window.console.log ("ERROR: eval_js \"{0}\"".format("%s"));
          } |}
      expr
      id
  in
  ignore (Js_of_ocaml.Js.Unsafe.eval_string expr)
;;

let show_modal () = eval_js "launch_modal" "$('#errorModal').modal('show')"
let hide_modal () = eval_js "hide_modal" "$('#errorModal').modal('hide')"

let apply_action (model : Model.t) (action : Action.t) _state ~schedule_action : Model.t
    =
  match action with
  | Log error ->
    Log.error error.detail;
    schedule_action Action.Open;
    let time = Browser.Date.(now () |> to_locale_time_string) in
    {model with lst = {seen = false; error; time} :: model.lst}
  | Open ->
    show_modal ();
    {model with visible = true}
  | Close ->
    hide_modal ();
    let lst = List.map model.lst ~f:(fun e -> {e with seen = true}) in
    {visible = false; lst}
;;

let modal ~inject contents =
  let open Vdom in
  let open Node in
  div
    Attr.
      [ class_ "modal"
      ; id "errorModal"
      ; tabindex (-1)
      ; create "role" "modal"
      ; create "data-backdrop" "static"
      ; create "aria-labelledby" "errorModalLabel"
      ; create "aria-hidden" "true" ]
    [ div
        Attr.[class_ "modal-dialog"; create "role" "document"]
        [ div
            Attr.[class_ "modal-content"]
            [ div
                Attr.[class_ "modal-header"]
                [ h5 Attr.[class_ "modal-title"; id "errorModalLabel"] [text "Fehler"]
                ; button
                    Attr.
                      [ type_ "button"
                      ; class_ "close"
                      ; on_click (fun _ -> inject Action.Close)
                      ; create "aria-label" "Close" ]
                    [span Attr.[create "aria-hidden" "true"] [text "×"]] ]
            ; div Attr.[class_ "modal-body"] contents
            ; div
                Attr.[class_ "modal-footer"]
                [ button
                    Attr.
                      [ type_ "button"
                      ; classes ["btn"; "btn-secondary"]
                      ; on_click (fun _ -> inject Action.Close) ]
                    [text "Schließen"] ] ] ] ]
;;

let view ~inject model =
  let%map (model : Model.t) = model in
  let f (e : Model.entry) =
    let open Vdom in
    let open Node in
    if not e.seen
    then
      [ p
          [Attr.class_ "text-monospace"]
          [text e.time; text ": "; text (Error.to_string_hum e.error.detail)]
      ; p [Attr.class_ "font-weight-bold"] [text e.error.gist] ]
    else []
  in
  modal ~inject List.(concat_map ~f model.lst |> rev)
;;

let create 
    :  inject:(Action.t -> Vdom.Event.t)
    -> Model.t Incr.t
    -> (Action.t, Model.t, State.t) Component.t Incr.t =
 fun ~inject model ->
  let%map model = model
  and view = view ~inject model in
  let apply_action = apply_action model in
  Component.create ~apply_action model view
;;

let _ = Action.Close, Action.Open
