module Icons exposing (..)

-- This module exposes font awesome icons for easy use.
-- It will grow over time. Check http://fontawesome.io/cheatsheet/
-- too find new stuff.


import Html exposing (Html)
import Html.Attributes as Attr


-- Helper function
i : String -> Html msg
i class =
  let class_ = "fa fa-" ++ class in
  Html.i [Attr.class class_] []


-- Definitions

save = i "floppy-o"

prev = i "angle-left"
next = i "angle-right"

