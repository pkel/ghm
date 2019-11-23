open Incr_dom.Vdom
open Core_kernel

let tab_skip = Attr.create "tabindex" "-1"

type icon =
  | B of string
  | R of string
  | S of string
  | L of string

let classes_of_icon = function
  | B s -> [ "fab"; sprintf "fa-%s" s ]
  | R s -> [ "far"; sprintf "fa-%s" s ]
  | S s -> [ "fas"; sprintf "fa-%s" s ]
  | L s -> [ "fal"; sprintf "fa-%s" s ]
;;

let icon i = Node.create "i" [ Attr.classes (classes_of_icon i) ] []

let button ?i ?(attr = []) ?(style = "secondary") ~action label =
  let style = sprintf "btn-%s" style in
  Node.button
    (attr
    @ [ Attr.on_click action; Attr.classes [ "btn"; style ]; Attr.type_ "button" ]
    @
    match i with
    | None -> []
    | Some _ -> [ Attr.create "title" label ])
    [ (match i with
      | None -> Node.text label
      | Some i -> icon i)
    ]
;;

let button' ?i ?(attr = []) ?(style = "secondary") ?(blank = false) ~href label =
  let style = sprintf "btn-%s" style in
  Node.a
    (attr
    @ [ Attr.href href; Attr.classes [ "btn"; style ]; Attr.create "role" "button" ]
    @ (match i with
      | None -> []
      | Some _ -> [ Attr.create "title" label ])
    @
    if blank
    then [ Attr.create "target" "_blank"; Attr.create "rel" "noopener norefferer" ]
    else [])
    [ (match i with
      | None -> Node.text label
      | Some i -> icon i)
    ]
;;

let button_clipboard ?i ?(attr = []) ?(style = "secondary") ~value label =
  let style = sprintf "btn-%s" style in
  let id = Base.(sprintf "clipboard-%i" (Hash.Builtin.hash_string value)) in
  Node.div
    []
    [ Node.button
        (attr
        @ [ Attr.create "data-clipboard-target" ("#" ^ id)
          ; Attr.classes [ "btn"; "clipboard-js"; style ]
          ]
        @
        match i with
        | None -> []
        | Some _ -> [ Attr.create "title" label ])
        [ (match i with
          | None -> Node.text label
          | Some i -> icon i)
        ]
    ; Node.textarea
        [ Attr.id id
        ; Attr.style
            Css_gen.(
              concat
                [ opacity 0.
                ; position ~left:(`Px 0) ~top:(`Px 0) `Absolute
                ; z_index (-1)
                ; height (`Px 0)
                ; width (`Px 0)
                ; overflow `Hidden
                ; border ~style:`None ()
                ])
        ]
        [ Node.text value ]
    ]
;;

let button_submit label =
  Node.button
    [ Attr.classes [ "btn"; "btn-secondary" ]; Attr.type_ "submit" ]
    [ Node.text label ]
;;

module Grid = struct
  module A = Attr
  module N = Node
  open N
  open A

  let div = div
  let row ?(c = []) = div [ classes ("row" :: c) ]
  let frow ?(c = []) = div [ classes ("form-row" :: c) ]
  let col ?(c = []) = div [ classes ("col" :: c) ]
  let col1 ?(c = []) = div [ classes ("col-1" :: c) ]
  let col2 ?(c = []) = div [ classes ("col-2" :: c) ]
  let col3 ?(c = []) = div [ classes ("col-3" :: c) ]
  let col4 ?(c = []) = div [ classes ("col-4" :: c) ]
  let col5 ?(c = []) = div [ classes ("col-5" :: c) ]
  let col6 ?(c = []) = div [ classes ("col-6" :: c) ]
  let col7 ?(c = []) = div [ classes ("col-7" :: c) ]
  let col8 ?(c = []) = div [ classes ("col-8" :: c) ]
  let col9 ?(c = []) = div [ classes ("col-9" :: c) ]
  let col10 ?(c = []) = div [ classes ("col-10" :: c) ]
  let col11 ?(c = []) = div [ classes ("col-11" :: c) ]
  let col_auto ?(c = []) = div [ classes ("col-auto" :: c) ]

  let loading_row =
    row
      ~c:[ "justify-content-center"; "align-items-center"; "pt-5" ]
      [ col_auto
          [ div
              [ Attr.class_ "spinner-border"; Attr.create "role" "status" ]
              [ span [ Attr.class_ "sr-only" ] [ Node.text "Lädt..." ] ]
          ]
      ]
  ;;
end

module Form = struct
  open Incr_dom
  open Vdom
  open Incr_dom_widgets.Interactive

  module Primitives = struct
    let shared_setup =
      let incr =
        let counter = ref 0 in
        fun () ->
          incr counter;
          "ghm_form_" ^ Int.to_string !counter
      in
      fun ~id ->
        let key = incr () in
        key, Option.value id ~default:key
    ;;

    let generic ~render ?(init = "") ?id () =
      let open Incr.Let_syntax in
      let key, id = shared_setup ~id in
      Primitives.create ~init ~render:(fun ~inject ~value ->
          let%map value = value in
          let on_input = Attr.on_input (fun _ev text -> inject text) in
          let attrs = [ Attr.id id; on_input ] in
          render ~key ~attrs ~value)
    ;;
  end

  let input
      ?(validator = fun _ -> None)
      ?(prepend = [])
      ?(append = [])
      ?placeholder
      ?init
      ?label
      ()
    =
    let label = Option.map ~f:(fun l -> Node.label [] [ Node.text l ]) label in
    let err msg = Node.div [ Attr.class_ "invalid-feedback" ] [ Node.text msg ] in
    let group input =
      match append, prepend with
      | [], [] -> input
      | _ ->
        Node.div
          [ Attr.class_ "input-group" ]
          (List.concat
             [ (match prepend with
               | [] -> []
               | l -> [ Node.div [ Attr.class_ "input-group-prepend" ] l ])
             ; [ input ]
             ; (match append with
               | [] -> []
               | l -> [ Node.div [ Attr.class_ "input-group-append" ] l ])
             ])
    in
    let render ~key ~attrs ~value =
      let attrs classes =
        let tl =
          Attr.classes ("form-control" :: classes)
          :: Attr.type_ "text"
          :: Attr.value value
          :: attrs
        in
        match placeholder with
        | None -> tl
        | Some p -> Attr.placeholder p :: tl
      in
      let nodes =
        match validator value with
        | None -> [ label; Some (Node.input ~key (attrs []) [] |> group) ]
        | Some msg ->
          [ label
          ; Some (Node.input ~key (attrs [ "is-invalid" ]) [] |> group)
          ; Some (err msg)
          ]
      in
      [ Node.div [ Attr.class_ "form-group" ] (List.filter_opt nodes) ]
    in
    Primitives.generic ~render ?init ()
  ;;

  let textarea ?(validator = fun _ -> None) ~nrows ?placeholder ?init label =
    let label = Node.label [] [ Node.text label ] in
    let err msg = Node.div [ Attr.class_ "invalid-feedback" ] [ Node.text msg ] in
    let render ~key ~attrs ~value =
      let attrs classes =
        let tl =
          Attr.classes ("form-control" :: classes)
          :: Attr.create "rows" (Int.to_string nrows)
          :: attrs
        in
        match placeholder with
        | None -> tl
        | Some p -> Attr.placeholder p :: tl
      in
      let nodes =
        match validator value with
        | None -> [ label; Node.textarea ~key (attrs []) [ Node.text value ] ]
        | Some msg ->
          [ label
          ; Node.textarea ~key (attrs [ "is-invalid" ]) [ Node.text value ]
          ; err msg
          ]
      in
      [ Node.div [ Attr.class_ "form-group" ] nodes ]
    in
    Primitives.generic ~render ?init ()
  ;;
end
