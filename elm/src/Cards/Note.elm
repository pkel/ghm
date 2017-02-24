module Cards.Note exposing
    ( Model
    , Cfg
    , Msg
    , view
    , edit
    , show
    , extract
    , update
    )

import Material
import Material.Card as Card
import Material.Textfield as Textfield
import Material.Options as Options

import Defaults exposing (..)

import Markdown

import Html exposing (Html, text)
import Html.Attributes as Attributes

type alias Model =
    { editMode : Bool
    , cache : String
    }

type alias Cfg msg =
    { mdl        : Material.Model
    , mdlMessage : (Material.Msg msg -> msg)
    , msg        : Msg -> msg
    , title      : String
    , edit       : msg
    , done       : msg
    , delete     : msg
    }

type Msg
    = CacheChanged String
    | Abort

show : Model
show =
    { editMode = False
    , cache = ""
    }

edit : String -> Model
edit note =
    { editMode = True
    , cache = note
    }

extract : Model -> Maybe String
extract model =
    case model.editMode of
        True -> Just model.cache
        False -> Nothing

update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        CacheChanged cache_ ->
            ( { model | cache = cache_ }, Cmd.none )

        Abort ->
            ( show, Cmd.none )


viewEdit : Cfg msg -> Model -> Html msg
viewEdit cfg model =
    let textfield =
            Textfield.render cfg.mdlMessage [0] cfg.mdl
                [ Textfield.value model.cache
                , Textfield.textarea
                , Options.css "width" "100%"
                , Textfield.rows (model.cache |> String.lines |> List.length)
                , Options.onInput (\t -> cfg.msg (CacheChanged t))
                ] ()

        defaultButton_ = defaultButton cfg.mdlMessage cfg.mdl

        actions = [ defaultButton_ "done" cfg.done
                  , defaultButton_ "cancel" (cfg.msg Abort)
                  , defaultButton_ "delete" cfg.delete
                  ]

        cardContent =
            [ Card.title [ defaultCardTitle ] [ text cfg.title ]
            , Card.text [] [ textfield ]
            , Card.actions [ defaultActions ] actions
            ]

    in
        Card.view [ defaultCard ] cardContent

viewShow : Cfg msg -> Model -> String -> Html msg
viewShow cfg model note =
    let mdDefaults = Markdown.defaultOptions

        mdOptions =
            { mdDefaults
            | githubFlavored = Just { tables = True, breaks = False }
            , sanitize = True
            , smartypants = True
            }

        mdToHtml =
            Markdown.toHtmlWith mdOptions
                [ Attributes.class "ghm_md_note" ]

        defaultButton_ = defaultButton cfg.mdlMessage cfg.mdl

        cardContent =
            case note of
                "" ->
                    [ Card.actions [ Options.center ]
                        [ defaultButton_ "note_add" cfg.edit ]
                    ]
                _  ->
                    [ Card.title [ defaultCardTitle ] [ text cfg.title ]
                    , Card.text [] [ mdToHtml note ]
                    , Card.actions [ defaultActions ]
                        [ defaultButton_ "mode_edit" cfg.edit
                        , defaultButton_ "delete" cfg.delete
                        ]
                    ]
    in
        Card.view [ defaultCard ] cardContent

view : Cfg msg -> Model -> String -> Html msg
view cfg model note =
    case model.editMode of
        True -> viewEdit cfg model
        False -> viewShow cfg model note

