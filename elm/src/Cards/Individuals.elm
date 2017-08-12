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

import Date exposing (Date)
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
    , birth  = Maybe.map (DateF.format "%Y-%m-%d") x.date_of_birth
        |> Maybe.withDefault ""
    }

type alias Model =
    { editMode : Bool
    , cache    : Array CacheItem
    , lst      : List Individual
    }

type alias Cfg msg =
    { mdl    : Material.Model
    , mdlMsg : Material.Msg msg -> msg
    , mdlIdx : List Int
    , msg    : Msg msg -> msg
    , return : List Booking.Individual -> msg
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
    | Abort
    -- Callbacks
    | Done (List Individual -> msg)
    | Mdl  (Material.Msg msg -> msg) (Material.Msg msg)

showMdl : List Individual -> Model
showMdl lst =
    { editMode = False
    , cache = Array.empty
    , lst = lst
    }

edit : Model -> Model
edit model =
    { model
    | editMode = True
    , cache = Array.fromList (List.map initCacheItem model.lst)
    }

model : List Individual -> Model
model = showMdl

dateFormatHint : String
dateFormatHint = "1995-04-15"

extractBirth : String -> Result String (Maybe Date)
extractBirth str =
    let str_ = String.trim str in
    case String.length str_ == 0 of
        True -> Ok Nothing
        False ->
            case Date.fromString str_ of
                Ok date -> Ok (Just date)
                Err err -> Err err

extract : Model -> Maybe (List Individual)
extract model =
    let birth str =
            case extractBirth str of
                Err err -> Nothing
                Ok mbDate -> mbDate

        f el =
            extractBirth el.birth
            |> Result.map ( \x ->
                { given = String.trim el.given
                , family = String.trim el.family
                , date_of_birth = x
                } )

        fold el acc = Result.map2 (::) (f el) (acc)
    in
        Array.toList model.cache
        |> List.foldl fold (Ok [])
        |> Result.map List.reverse
        |> Result.toMaybe


updateItem : ItemMsg -> CacheItem -> CacheItem
updateItem msg item =
    case msg of
        Given str ->
            { item | given = str }
        Family str ->
            { item | family = str }
        Birth str ->
            { item | birth = str }

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
            in
            ( { model | cache = cache_ }, Cmd.none )


        Abort ->
            ( { model | editMode = False}  , Cmd.none )

        Edit lst ->
            -- Switch to edit mode
            ( edit model , Cmd.none )

        -- callbacks

        Done cmd ->
            extract model |>
            Maybe.map (
                \x -> (showMdl x, Task.perform cmd (Task.succeed x) )
                ) |>
            -- TODO: notify reason
            Maybe.withDefault (model, Cmd.none)

        Mdl cmd msg -> (model, Task.perform cmd (Task.succeed msg))

viewEdit : Cfg msg -> Model -> Html (Msg msg)
viewEdit cfg model =
    let mdlMsg = Mdl cfg.mdlMsg

        id x = (x :: cfg.mdlIdx)

        field i label up show check hint (nth,el) =
            let val = show el
                props =
                [ Options.onInput (\x -> ItemChange nth (up x))
                , Textfield.label label
                , Textfield.value val
                , Textfield.error hint |> Options.when (not <| check val)
                , Options.css "width" "auto"
                , Options.css "padding-top" "0"
                , Options.css "padding-bottom" "1"
                , Options.css "font-size" "13px"
                ]
            in
            Textfield.render mdlMsg (nth::(id i)) cfg.mdl
                props []

        true str = True

        checkBirth str = case extractBirth str of
            Ok _  -> True
            Err _ -> False

        given  = field 201 "" Given  .given  true       ""
        family = field 202 "" Family .family true       ""
        birth  = field 203 "" Birth  .birth  checkBirth dateFormatHint

        delete (i, _)  =
            defaultButtonMini mdlMsg cfg.mdl (i::(id 204)) "delete"
                (ItemDelete i)

        defaultButton_ = defaultButton mdlMsg cfg.mdl

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
            defaultButtonMini mdlMsg cfg.mdl (id 100) "add" ItemAdd

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
            [ defaultButton_ (id 302) "cancel" Abort
            , defaultButton_ (id 303) "done"   (Done cfg.return)
            ]
    in
        Card.view
            [ defaultCard ]
            [ Card.title [ Options.center ] [ table ]
            , Card.actions [ defaultActions ] actions
            ]

viewShow : Cfg msg -> Model -> Html (Msg msg)
viewShow cfg mdl =
    let mdlMsg = Mdl cfg.mdlMsg

        lst = mdl.lst

        birth i = text
            ( Maybe.withDefault "n/a"
                ( Maybe.map (DateF.format "%d.%m.%Y") i.date_of_birth)
            )

        given i = text i.given
        family i = text i.family

        defaultButton_ = defaultButton mdlMsg cfg.mdl
        i x = (x :: cfg.mdlIdx)

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
            [ defaultButton_ (i 102) "mode_edit" (Edit lst)
            ]
    in
        Card.view
            [ defaultCard ]
            [ Card.title [ Options.center ] [ table ]
            , Card.actions [ defaultActions ] actions
            ]

view : Cfg msg -> Model -> Html msg
view cfg model =
    Html.map cfg.msg <| case model.editMode of
        True -> viewEdit cfg model
        False -> viewShow cfg model

