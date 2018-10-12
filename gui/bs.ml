open Incr_dom.Vdom
open Core_kernel

let button ~action label =
  Node.button
    [ Attr.on_click action
    ; Attr.classes ["btn"; "btn-secondary"]
    ; Attr.type_ "button" ]
    [Node.text label]

let button' ~href label =
  Node.a
    [ Attr.href href
    ; Attr.classes ["btn"; "btn-secondary"]
    ; Attr.create "role" "button" ]
    [Node.text label]

let row divs =
  let f node = Node.div [Attr.classes ["col"]] [node] in
  Node.div [Attr.class_ "row"] (List.map ~f divs)

let rows divs = List.map ~f:row divs
