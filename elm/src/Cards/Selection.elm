-- Stateless card. No callback/update/model

module Cards.Selection exposing
    ( view
    , table
    , Cfg
    )

import Material
import Material.Card as Card
import Material.Options as Options

import Defaults

import Html exposing (Html, text, div)
import Html.Attributes as Attributes

import Array exposing (Array)

import Material.Table as Table

-- View

type alias Cfg msg element =
    { title  : Maybe String
    , render : Render msg element
    , index  : List Int
    , mdl    : Material.Msg msg -> msg
    , add    : msg
    , select : Int -> msg
    }

type alias Render msg element =
    { row    : (Int -> msg) -> Int -> Int -> element -> Html msg
    , fold   : List (Html msg) -> Html msg
    }

table : List ( Html msg, (element -> String) ) -> Render msg element
table fields =
    let (hd, fieldFns ) = List.unzip fields
        c = Options.css "text-align" "center"
        row select focus i el =
            Table.tr
                [ Options.onClick (select i)
                , Table.selected
                    |> Options.when (focus == i)
                ]
                ( List.map (\f -> Table.td [c] [text (f el)]) fieldFns )

        head = List.map (\x -> Table.th [c] [x]) hd
            |> Table.thead []

        fold rows = Table.table [] [ head, Table.tbody [] rows ]
    in
    { row  = row
    , fold = fold
    }

view : Cfg msg element -> Material.Model -> Array element -> Int -> Html msg
view cfg mdl data focus =
    -- rows
    Array.indexedMap (\i el -> cfg.render.row cfg.select focus i el) data
    |> Array.toList
    -- rows combined with header
    |> cfg.render.fold
    -- lower part of card
    |> \x ->
        [ Card.title [ Options.center ] [ x ]
        , Card.actions [ Defaults.actions ] [
            Defaults.button cfg.mdl mdl (30 :: cfg.index) "add" cfg.add ]
        ]
    -- add title if given
    |> \x ->
        ( case cfg.title of
            Nothing -> x
            Just str -> Card.title [ Defaults.cardTitle ] [ text str ] :: x
        )
    -- complete card
    |> Card.view [ Defaults.card ]
