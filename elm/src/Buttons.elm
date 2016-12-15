module Buttons exposing (..)

-- This module will be used for button generation.
-- We abstract to enforce tooltips and uniform design

import Icons
import Pure
import Html exposing (Html)
import Html.Events as E
import Html.Attributes as A

button : String -> List (Html msg) -> msg -> Html msg
button tooltip contains action =
  Pure.button [E.onClick action, A.title tooltip] contains

-- All these things are of type
-- msg -> Html msg
-- but i am too lazy to type this

prev = button "Vorheriger Eintrag" [Icons.prev]
next = button "Nächster Eintrag"   [Icons.next]
save = button "Eintrag Speichern"  [Icons.save]
last = button "Letzter Eintrag"    [Icons.last]
add  = button "Eintrag hinzufügen" [Icons.add]

