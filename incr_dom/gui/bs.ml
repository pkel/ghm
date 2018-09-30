open Incr_dom.Vdom

let button action label =
  Node.button [ Attr.on_click action
              ; Attr.class_ "btn"
              ; Attr.type_ "button"
              ] [ Node.text label ]
