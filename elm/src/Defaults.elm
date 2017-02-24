module Defaults exposing (..)

import Material
import Material.Elevation as Elevation
import Material.Button as Button
import Material.Icon as Icon
import Material.Options as Options
import Material.Typography as Typography
import Material.Card as Card

import Html exposing (Html)

defaultCard : Options.Property c m
defaultCard =
    Options.many
        [ Elevation.e2
        , Options.css "margin" "0.5em"
        , Options.css "width" "auto"
        ]

defaultActions : Options.Property () m
defaultActions =
    Options.many
        [ Options.center
        , Card.border
        ]

defaultButton : (Material.Msg msg -> msg) -> Material.Model -> String -> msg -> Html msg
defaultButton eMdl mdl icon action =
    Button.render eMdl [0] mdl
        [ Button.colored
        , Button.raised
        , Button.minifab
        , Options.onClick action
        , Options.css "margin" "0 4px 0 4px"
        ]
        [ Icon.i icon ]

defaultCardTitle : Options.Property c m
defaultCardTitle =
    Options.many
        [ Typography.title
        -- , Options.center
        -- , Color.text Color.primaryDark
        ]


