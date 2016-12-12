module Pure exposing (..)

import Html as H exposing (Html, Attribute)
import Html.Attributes as A

addClass : (List (Attribute msg) -> List (Html msg) -> Html msg) -> String -> List (Attribute msg) -> List (Html msg) -> Html msg
addClass f class a =
  f ((A.class class) :: a)

text     = H.text
legend   = H.legend
fieldset = H.fieldset
label    = H.label
input    = H.input
br       = H.br
div      = H.label

textarea a = addClass H.textarea "pure-input"  a
button   a = addClass H.button   "pure-button" a
form     a = addClass H.form     "pure-form"   a

-- Grid
u n outOf a =
  addClass div ("pure-" ++ (toString n) ++ "-" ++ (toString outOf)) a

g a = addClass div "pure-g"

u1_1 a = u 1 1 a
u1_2 a = u 1 2 a
u1_3 a = u 1 3 a
u2_3 a = u 2 3 a

