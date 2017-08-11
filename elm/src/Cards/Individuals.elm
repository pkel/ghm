module Cards.Individuals exposing
    ( Model
    , Cfg
    , Msg
    , view
    , model
    , update
    )

import Material
import Material.Card as Card
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Table as Table

import Booking exposing (Individual)

import Defaults exposing (..)

import Html exposing (Html, text)
import Html.Attributes as Attributes

import Helpers.Array as ArrayX

import Array exposing (Array)

import Date.Format as DateF

import Task

type alias CacheItem =
    { given  : String
    , family : String
    , birth  : String
    }

emptyCacheItem : CacheItem
emptyCacheItem =
    { given  = ""
    , family = ""
    , birth  = ""
    }

initCacheItem : Individual -> CacheItem
initCacheItem x =
    { given  = x.given
    , family = x.family
    -- TODO: day of birth editing
    , birth  = ""
    }

type alias Model =
    { editMode : Bool
    , focus : Int
    , cache : Array CacheItem
    }

type alias Cfg msg =
    { mdl     : Material.Model
    , mdlMsg  : Material.Msg msg -> msg
    , msg     : Msg msg -> msg
    , index   : List Int
    , title   : String
    , updated : List Booking.Individual -> msg
    }

type ItemMsg
    = Given  String
    | Family String
    | Birth  String

type Msg msg
    = ItemChange Int ItemMsg
    | ItemDelete Int
    | ItemAdd
    | Edit (List Booking.Individual)
    | Done (List Individual -> msg)
    | Abort

showMdl : Model
showMdl =
    { editMode = False
    , focus = 0
    , cache = Array.empty
    }

model : Model
model = showMdl

editMdl : List Individual -> Model
editMdl lst =
    { editMode = True
    , focus = 0
    , cache = Array.fromList (List.map initCacheItem lst)
    }

-- TODO: This should check the data for errors
extract : Model -> Maybe (List Individual)
extract model =
    case model.editMode of
        True -> Just []
        -- True -> Just (Array.toList model.cache)
        False -> Nothing

updateItem : ItemMsg -> CacheItem -> CacheItem
updateItem msg item =
    case msg of
        Given str ->
            { item | given = str }
        Family str ->
            { item | family = str }
        Birth str ->
            -- TODO: date of birth editing
            item

update : Msg msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ItemChange index itemMsg ->
            let cache_ =
                    Array.get index model.cache |>
                    Maybe.map (\x -> updateItem itemMsg x) |>
                    Maybe.map (\x -> Array.set index x model.cache) |>
                    Maybe.withDefault model.cache
            in
            ( { model | cache = cache_ }, Cmd.none )

        ItemDelete index ->
            let cache_ =
                    ArrayX.delete index model.cache
            in
            ( { model | cache = cache_ }, Cmd.none )

        ItemAdd ->
            let cache_ =
                    Array.push (initCacheItem Booking.emptyIndividual) model.cache

                focus_ = Array.length cache_
            in
            ( { model | cache = cache_, focus = focus_ }, Cmd.none )

        Done cmd ->
            extract model |>
            Maybe.map (
                \x -> Task.succeed x |>
                \x -> (showMdl, Task.perform cmd x)
                ) |>
            -- TODO: notify reason
            Maybe.withDefault (model, Cmd.none)


        Abort ->
            ( showMdl , Cmd.none )

        Edit lst ->
            -- Switch to edit mode
            ( editMdl lst , Cmd.none )

viewEdit : Cfg msg -> Model -> Html msg
viewEdit cfg model =
    let id x = (x :: cfg.index)

        field i label up f (nth,el) =
            Textfield.render cfg.mdlMsg (nth::(id i)) cfg.mdl
                [ Options.onInput (\x -> cfg.msg (ItemChange nth (up x)))
                , Options.css "width" "auto"
                , Options.css "padding-top" "0"
                , Options.css "padding-bottom" "0"
                , Options.css "font-size" "13px"
                , Textfield.label label
                , Textfield.value (f el)
                ] []

        given  = field 201 ""           Given  .given
        family = field 202 ""           Family .family
        birth  = field 203 "__.__.____" Birth  .birth

        delete (i, _)  =
            defaultButtonMini cfg.mdlMsg cfg.mdl (i::(id 204)) "delete"
                (cfg.msg (ItemDelete i))

        defaultButton_ = defaultButton cfg.mdlMsg cfg.mdl

        left  = Options.css "text-align" "left"
        right = Options.css "text-align" "right"

        row i =
            Table.tr []
                [ Table.td [left ] [given  i]
                , Table.td [left ] [family i]
                , Table.td [right] [birth  i]
                , Table.td []      [delete i]
                ]

        lst = model.cache |> Array.toIndexedList

        add =
            defaultButtonMini cfg.mdlMsg cfg.mdl (id 100) "add"
                (cfg.msg ItemAdd)

        table =
            Table.table []
                [ Table.thead []
                    [ Table.tr []
                        [ Table.th [left ] [text "Vorname"]
                        , Table.th [left ] [text "Name"]
                        , Table.th [right] [text "Geburtsdatum"]
                        , Table.th [] [ add ]
                        ]
                    ]
                , Table.tbody [] (List.map row lst)
                ]

        actions =
            [ defaultButton_ (id 302) "cancel" (cfg.msg Abort)
            , defaultButton_ (id 303) "done"   (cfg.msg (Done cfg.updated))
            ]
    in
        Card.view
            [ defaultCard ]
            [ Card.title [ defaultCardTitle ] [ text cfg.title ]
            , Card.title [ Options.center ] [ table ]
            , Card.actions [ defaultActions ] actions
            ]

viewShow : Cfg msg -> List Individual -> Html msg
viewShow cfg lst =
    let birth i = text
            ( Maybe.withDefault "n/a"
                ( Maybe.map (DateF.format "%d.%m.%Y") i.date_of_birth)
            )

        given i = text i.given
        family i = text i.family

        defaultButton_ = defaultButton cfg.mdlMsg cfg.mdl
        i x = (x :: cfg.index)

        left  = Options.css "text-align" "left"
        right = Options.css "text-align" "right"

        row i =
            Table.tr []
                [ Table.td [left ] [given i]
                , Table.td [left ] [family i]
                , Table.td [right] [birth i]
                ]

        table =
            Table.table []
                [ Table.thead []
                    [ Table.tr []
                        [ Table.th [left ] [text "Vorname"]
                        , Table.th [left ] [text "Name"]
                        , Table.th [right] [text "Geburtsdatum"]
                        ]
                    ]
                , Table.tbody [] (List.map row lst)
                ]

        actions =
            -- [ defaultButton_ (i 101) "add"       (cfg.msg CacheItemAdd)
            [ defaultButton_ (i 102) "mode_edit" (cfg.msg (Edit lst))
            ]
    in
        Card.view
            [ defaultCard ]
            [ Card.title [ defaultCardTitle ] [ text cfg.title ]
            , Card.title [ Options.center ] [ table ]
            , Card.actions [ defaultActions ] actions
            ]

view : Cfg msg -> Model -> List Individual -> Html msg
view cfg model lst =
    case model.editMode of
        True -> viewEdit cfg model
        False -> viewShow cfg lst

