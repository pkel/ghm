module Cards.Note exposing
    ( Model
    , Cfg
    , Msg
    , init
    , view
    , update
    )

import Material
import Material.Helpers exposing (pure, effect, cmd)
import Material.HelpersX exposing (callback)
import Material.Card as Card
import Material.Textfield as Textfield
import Material.Options as Options

import Defaults exposing (..)

import Markdown

import Html exposing (Html, text)
import Html.Attributes as Attributes

type alias Model =
    { editing : Bool
    , data    : String
    , cache   : String
    }

type alias Cfg msg =
    { title : String
    , lift : Msg msg -> msg
    , index : List Int
    }

type Msg msg
    = Change String
    | Edit
    | Done
    | Delete
    | Abort
    | Mdl (Material.Msg msg)

type alias DataCb msg = String -> msg
type alias MdlCb  msg = Material.Msg msg -> msg

init : String -> Model
init str =
    { editing = False
    , cache = ""
    , data = str
    }

edit : Model -> Model
edit model =
    { model
    | cache = model.data
    , editing = True
    }

update : MdlCb m -> DataCb m -> Msg m -> Model -> ( Model, Cmd m )
update mdlCb dataCb msg model =
    case msg of
        Change str ->
            pure { model | cache = str }

        Abort ->
            pure { model | editing = False }

        Edit ->
            edit model |> pure

        Delete ->
            init "" |> callback dataCb ""

        Done ->
            String.trim model.cache
            |> (\x -> init x |> callback dataCb x)

        Mdl msg ->
            callback mdlCb msg model

-- View

view : Cfg msg -> Material.Model -> Model -> Html msg
view cfg mdl model =
    Html.map cfg.lift <| case model.editing of
        True  -> viewEdit cfg mdl model
        False -> viewShow cfg mdl model


viewEdit : Cfg msg -> Material.Model -> Model -> Html (Msg msg)
viewEdit cfg mdl model =
    let i x = (x :: cfg.index)
        textfield =
            Textfield.render Mdl (i 1) mdl
                [ Textfield.value model.cache
                , Textfield.textarea
                , Options.css "width" "100%"
                , Textfield.rows (model.cache |> String.lines |> List.length)
                , Options.onInput Change
                ] ()

        defaultButton_ = defaultButton Mdl mdl

        actions = [ defaultButton_ (i 2) "done"   Done
                  , defaultButton_ (i 3) "cancel" Abort
                  , defaultButton_ (i 4) "delete" Delete
                  ]

        cardContent =
            [ Card.title [ defaultCardTitle ] [ text cfg.title ]
            , Card.text [] [ textfield ]
            , Card.actions [ defaultActions ] actions
            ]

    in
        Card.view [ defaultCard ] cardContent

viewShow : Cfg msg -> Material.Model -> Model -> Html (Msg msg)
viewShow cfg mdl model =
    let mdToHtml =
            Markdown.toHtmlWith Defaults.markdown
                [ Attributes.class "ghm_md_note" ]

        defaultButton_ = defaultButton Mdl mdl

        i x = (x :: cfg.index)

        cardContent =
            case model.data of
                "" ->
                    [ Card.actions [ Options.center ]
                        [ defaultButton_ (i 5) "note_add" Edit ]
                    ]
                _  ->
                    [ Card.title [ defaultCardTitle ] [ text cfg.title ]
                    , Card.text [] [ mdToHtml model.data ]
                    , Card.actions [ defaultActions ]
                        [ defaultButton_ (i 6) "mode_edit" Edit
                        , defaultButton_ (i 7) "delete" Delete
                        ]
                    ]
    in
        Card.view [ defaultCard ] cardContent

