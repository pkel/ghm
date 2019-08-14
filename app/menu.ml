type t = entry list

and entry =
  { action : action
  ; title : string
  ; children : entry list
  ; active : bool
  }

and action =
  | Href of string
  | On_click of (unit -> Incr_dom.Vdom.Event.t)

let entry ?(children = []) title action active = { action; children; title; active }
