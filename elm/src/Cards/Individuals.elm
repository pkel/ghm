module Cards.Individuals exposing
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
import Material.Table as Table

import Booking exposing (Individual)

import Defaults exposing (..)

import Html exposing (Html, text)
import Html.Attributes as Attributes

import Helpers.Array as ArrayX

import Array exposing (Array)

import Date.Format as DateF

type alias CacheItem =
    { given         : String
    , family        : String
    , date_of_birth : String
    }

emptyCacheItem : CacheItem
emptyCacheItem =
    { given         = ""
    , family        = ""
    , date_of_birth = ""
    }

initCacheItem : Individual -> CacheItem
initCacheItem x =
    { given         = x.given
    , family        = x.family
    -- TODO: day of birth editing
    , date_of_birth = ""
    }

type alias Model =
    { editMode : Bool
    , focus : Int
    , cache : Array CacheItem
    }

type alias Cfg msg =
    { mdl     : Material.Model
    , mdlMsg  : Material.Msg msg -> msg
    , msg     : Msg -> msg
    , index   : List Int
    , title   : String
    , updated : List Booking.Individual -> msg
    }

type ItemMsg
    = Given         String
    | Family        String
    | Date_of_birth String

type Msg
    = ItemChange Int ItemMsg
    | ItemDelete Int
    | ItemAdd
    | Edit
    | Abort

show : Model
show =
    { editMode = False
    , focus = 0
    , cache = Array.empty
    }

edit : List Individual -> Model
edit lst =
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
        Date_of_birth str ->
            -- TODO: date of birth editing
            item

update : Msg -> Model -> ( Model, Cmd msg )
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

        Abort ->
            ( show, Cmd.none )

        Edit ->
            -- Switch to edit mode
            ( show, Cmd.none )

viewEdit : Cfg msg -> Model -> Html msg
viewEdit cfg model =
    -- TODO: if focus is in range then make field editable
    viewShow cfg []

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
            [ defaultButton_ (i 102) "mode_edit" (cfg.msg Edit)
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

