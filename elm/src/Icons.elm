module Icons exposing (..)

-- This module exposes font awesome icons for easy use.
-- It will grow over time. Check http://fontawesome.io/cheatsheet/
-- too find new stuff.


import Html exposing (Html)
import Html.Attributes as Attr


-- Helper function
icon : String -> Html msg
icon class =
  let class_ = "fa fa-" ++ class in
  Html.i [Attr.class class_] []


-- Definitions

save  = icon "floppy-o"

prev  = icon "angle-left"
next  = icon "angle-right"
last  = icon "angle-double-right"
first = icon "angle-double-first"
add   = icon "plus"

