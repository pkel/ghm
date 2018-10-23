open Incr_dom.Vdom
open Core_kernel

type icon = B of string | R of string | S of string | L of string

let classes_of_icon = function
  | B s -> ["fab"; sprintf "fa-%s" s]
  | R s -> ["far"; sprintf "fa-%s" s]
  | S s -> ["fas"; sprintf "fa-%s" s]
  | L s -> ["fal"; sprintf "fa-%s" s]

let icon i label =
  Node.create "i"
    [Attr.create "title" label; Attr.classes (classes_of_icon i)]
    []

let button ~action label =
  Node.button
    [ Attr.on_click action
    ; Attr.classes ["btn"; "btn-secondary"]
    ; Attr.type_ "button" ]
    [Node.text label]

let button' ?i ?(attr = []) ?(style = "secondary") ~href label =
  let style = sprintf "btn-%s" style in
  Node.a
    ( attr
    @ [Attr.href href; Attr.classes ["btn"; style]; Attr.create "role" "button"]
    )
    [(match i with None -> Node.text label | Some i -> icon i label)]

let submit label =
  Node.button
    [Attr.classes ["btn"; "btn-secondary"]; Attr.type_ "submit"]
    [Node.text label]

module Grid = struct
  module A = Attr
  module N = Node
  open N
  open A

  let div = div

  let row = div [class_ "row"]

  let frow = div [class_ "form-row"]

  let col ?(c = []) = div [classes ("col" :: c)]

  let col1 ?(c = []) = div [classes ("col-1" :: c)]

  let col2 ?(c = []) = div [classes ("col-2" :: c)]

  let col3 ?(c = []) = div [classes ("col-3" :: c)]

  let col4 ?(c = []) = div [classes ("col-4" :: c)]

  let col5 ?(c = []) = div [classes ("col-5" :: c)]

  let col6 ?(c = []) = div [classes ("col-6" :: c)]

  let col7 ?(c = []) = div [classes ("col-7" :: c)]

  let col8 ?(c = []) = div [classes ("col-8" :: c)]

  let col9 ?(c = []) = div [classes ("col-9" :: c)]

  let col10 ?(c = []) = div [classes ("col-10" :: c)]

  let col11 ?(c = []) = div [classes ("col-11" :: c)]
end
