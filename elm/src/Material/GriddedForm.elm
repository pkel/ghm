module Material.GriddedForm exposing (..)

import Material
import Material.Textfield as Textfield
import Material.Grid as Grid
import Material.Style as Style
import Material.Options as Options
import Material.List as Lists

import Html exposing (Html)
import Html.Attributes as Attributes

-- TODO: This module needs a lot of love

-- textfield : (Material.Msg m -> m) -> List Int -> Material.Model
   -- -> List a -> String -> (String -> m) -> String
   -- -> Html m
textfield mdlmsg idx mdl attr label action value  =
    Textfield.render mdlmsg idx mdl
        [ Options.many attr
        , Options.onInput action
        , Textfield.label label
        , Textfield.floatingLabel
        , Textfield.value value
        , Style.widthMax
        ] []

grid      = [ Style.box0 ] |> Grid.grid
cell attr = Style.marginVert0 :: attr |> Grid.cell

li = Lists.li [ Style.box0 ]
ul = Lists.ul [ Style.box0 ]

contain x = Html.div [ Attributes.style [("margin-top","1em")] ] [x]
