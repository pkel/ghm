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

let button_clipboard ?i ?(attr = []) ?(style = "secondary") ~id ~value label =
  let style = sprintf "btn-%s" style in
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
