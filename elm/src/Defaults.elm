module Defaults exposing (..)

import Material
import Material.Elevation as Elevation
import Material.Button as Button
import Material.Icon as Icon
import Material.Options as Options
import Material.Typography as Typography
import Material.Card as Card
import Material.Style as Style

import Markdown

import Html exposing (Html)

card : Options.Property c m
card =
    Options.many
        [ Elevation.e2
        , Options.css "margin" "0.5em"
        , Style.widthAuto
        ]

actions : Options.Property () m
actions =
    Options.many
        [ Options.center
        , Card.border
        ]

type alias Button a msg =
   (Material.Msg a -> msg)
   -> Material.Model
   -> List Int
   -> String
   -> msg
   -> Html msg

-- button_ : List (Options.Property c m) -> Button a msg
button_ cfg eMdl mdl index icon action =
    Button.render eMdl index mdl
        ( Options.many
            [ Button.colored
            , Options.onClick action
            , Options.css "margin" "0 4px 0 4px"
            ] :: cfg )
        [ Icon.i icon ]

button : Button a msg
button = button_ [ Button.raised ]

buttonMini : Button a msg
buttonMini = button_ [ Button.icon ]

cardTitle : Options.Property c m
cardTitle =
    Options.many
        [ Typography.title
        -- , Options.center
        -- , Color.text Color.primaryDark
        ]

markdown : Markdown.Options
markdown =
    let factory = Markdown.defaultOptions
    in
        { factory
        | githubFlavored = Just { tables = True, breaks = False }
        , sanitize = True
        , smartypants = True
        }




