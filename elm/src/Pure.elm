module Pure exposing (..)

import Html as H exposing (Html, Attribute)
import Html.Attributes as A
import Html.Events as E

addClass : (List (Attribute msg) -> List (Html msg) -> Html msg) ->
  String -> List (Attribute msg) -> List (Html msg) -> Html msg
addClass f class a =
  f ((A.class class) :: a)

classedDiv : String -> List (Html msg) -> Html msg
classedDiv class =
  addClass div class []

addStyle : (List (Attribute msg) -> List (Html msg) -> Html msg) ->
  List (String,String) -> List (Attribute msg) -> List (Html msg) -> Html msg
addStyle f stl a =
  f ((A.style stl) :: a)

text     = H.text
br       = H.br
div      = H.div

button a =
  addClass (H.button) "pure-button" ((A.type_ "button") :: a)

-- Grid
-- we make sure, that a pure-g only contains pure-u-*

type GridElement msg = BasicGridElement (Html msg)

element: Int -> Int -> List (Html msg) -> GridElement msg
element n outOf contains =
  let class = "pure-u-" ++ (toString n) ++ "-" ++ (toString outOf) in
  let div = classedDiv class in
  BasicGridElement (div contains)

group: List (GridElement msg) -> Html msg
group elements =
  let divList = List.map (\(BasicGridElement msg) -> msg) elements in
  let div = classedDiv "pure-g" in
  div divList

group2: Int -> List (List (Html msg), Int) ->  Html msg
group2 measure lst =
  let fn (contains, n) = element n measure contains in
  let lst_ = List.map fn lst in
  group lst_

-- Forms

fieldset   = H.fieldset
legend     = H.legend
label      = H.label
input      = H.input

form a =
  addClass H.form "pure-form pure-form-stacked" a

textarea a =
  addClass H.textarea "pure-input" ((A.style [("resize","vertical")]) :: a)

textfield : String -> (String -> msg) -> String -> Html msg
textfield prefill action value =
  input [ A.type_ "text"
        , E.onInput action
        , A.value value
        , A.placeholder prefill
        , A.class "pure-input-1"
        ] []

titledTextfield : String -> (String -> msg) -> String -> Html msg
titledTextfield title action value =
  -- TODO: Set id of input and for tag of label
  let div = classedDiv "pure-control-group" in
  let l = label [] [text title] in
  div [l, textfield "" action value]

