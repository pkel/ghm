module Material.GriddedForm exposing
    ( textfield
    , grid
    , cell
    , li
    , ul
    , contain
    )

import Material
import Material.Textfield as Textfield
import Material.Grid as Grid
import Material.Style as Style
import Material.Options as Options
import Material.List as Lists

import Html exposing (Html)
import Html.Attributes as Attributes

-- TODO: This module needs a lot of love

datalist : String -> List String -> Html m
datalist id l =
    let option s =
            Html.option [Attributes.attribute "value" s] []
    in
        List.map option l
        |> Html.datalist [Attributes.id id]

-- textfield : (Material.Msg m -> m) -> List Int -> Material.Model
   -- -> List a -> String -> (String -> m) -> String
   -- -> Html m
textfield mdlmsg idx mdl attr label data action value =
    let (hasdata, listid, extend) =
            case data of
                Nothing -> ( False , "", \x -> x )
                Just d ->
                    ( True
                    , List.map toString idx
                        |> String.join "."
                        |> (++) "datalist."
                    , \x -> Html.div [] [x, datalist listid d]
                    )
    in
        Textfield.render mdlmsg idx mdl
        [ Options.many attr
        , Options.onInput action
        , Options.attribute (Attributes.attribute "list" listid) |>
            Options.when hasdata
        , Textfield.label label
        , Textfield.floatingLabel
        , Textfield.value value
        , Style.widthMax
        ] []
        |> extend

grid      = [ Style.box0 ] |> Grid.grid
cell attr = Style.marginVert0 :: attr |> Grid.cell

li = Lists.li [ Style.box0 ]
ul = Lists.ul [ Style.box0 ]

contain x = Html.div [ Attributes.style [("margin-top","1em")] ] [x]
