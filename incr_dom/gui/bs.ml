open Incr_dom.Vdom
open Core_kernel

let button action label =
  Node.button [ Attr.on_click action
              ; Attr.class_ "btn"
              ; Attr.type_ "button"
              ] [ Node.text label ]

let row divs =
  let f node = Node.div [Attr.classes ["col"; "mb-2"]] [node] in
  Node.div [Attr.class_ "row"] (List.map ~f divs)
