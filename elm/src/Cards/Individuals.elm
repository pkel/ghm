module Cards.Individuals exposing
    ( Model
    , Msg
    , view
    , init
    , update
    )

import Material
import Material.Helpers exposing (pure, effect)
import Material.HelpersX exposing (callback)
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

type alias Data = List Individual
type alias MdlCb msg  = Material.Msg msg -> msg
type alias DataCb msg = Data -> msg

type alias Model =
    { editing : Bool
    , cache   : Array CacheItem
    , data    : Data
    }

type ItemMsg
    = Given  String
    | Family String
    | Birth  String

type Msg msg
    = ItemChange Int ItemMsg
    | ItemDelete Int
    | ItemAdd
    | Edit
    | Abort
    | Done
    | Mdl (Material.Msg msg)

init : Data -> Model
init data =
    { editing = False
    , cache   = Array.empty
    , data    = data
    }

edit : Model -> Model
edit model =
    { model
    | editing = True
    , cache   = Array.fromList (List.map initCacheItem model.data)
    }

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

extract : Model -> Maybe Data
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


-- Update


updateItem : ItemMsg -> CacheItem -> CacheItem
updateItem msg item =
    case msg of
        Given str ->
            { item | given = str }
        Family str ->
            { item | family = str }
        Birth str ->
            { item | birth = str }

update : MdlCb msg -> DataCb msg -> Msg msg -> Model -> ( Model, Cmd msg )
update mdlCb dataCb msg model =
    case msg of
        ItemChange index itemMsg ->
            let cache_ =
                    case Array.get index model.cache of
                        Nothing -> model.cache
                        Just el -> updateItem itemMsg el
                            |> \x -> Array.set index x model.cache
            in
            { model | cache = cache_ } |> pure

        ItemDelete index ->
            let cache_ =
                    ArrayX.delete index model.cache
            in
            { model | cache = cache_ } |> pure

        ItemAdd ->
            let cache_ =
                    model.cache |>
                    Array.push (initCacheItem Booking.emptyIndividual)
            in
            { model | cache = cache_ } |> pure

        Abort ->
            { model | editing = False , cache = Array.empty } |> pure

        Edit ->
            edit model |> pure

        Done ->
            case extract model of
                Nothing -> pure model
                Just data -> init data |> callback (dataCb data)

        Mdl msg -> model |> callback (mdlCb msg)


-- View


viewEdit : List Int -> Material.Model -> Model -> Html (Msg msg)
viewEdit   idx         mdl               model =
    let id x = (x :: idx)

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
            Textfield.render Mdl (nth::(id i)) mdl
                props []

        true str = True

        checkBirth str = case extractBirth str of
            Ok _  -> True
            Err _ -> False

        given  = field 201 "" Given  .given  true       ""
        family = field 202 "" Family .family true       ""
        birth  = field 203 "" Birth  .birth  checkBirth dateFormatHint

        delete (i, _)  =
            defaultButtonMini Mdl mdl (i::(id 204)) "delete"
                (ItemDelete i)

        defaultButton_ = defaultButton Mdl mdl

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
            defaultButtonMini Mdl mdl (id 100) "add" ItemAdd

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
            , defaultButton_ (id 303) "done"   Done
            ]
    in
        Card.view
            [ defaultCard ]
            [ Card.title [ Options.center ] [ table ]
            , Card.actions [ defaultActions ] actions
            ]

viewShow : List Int -> Material.Model -> Model -> Html (Msg msg)
viewShow   idx         mdl               model =
    let birth i = text
            ( Maybe.withDefault "n/a"
                ( Maybe.map (DateF.format "%d.%m.%Y") i.date_of_birth)
            )

        given i = text i.given
        family i = text i.family

        defaultButton_ = defaultButton Mdl mdl
        i x = (x :: idx)

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
                , Table.tbody [] (List.map row model.data)
                ]

        actions =
            [ defaultButton_ (i 102) "mode_edit" Edit
            ]
    in
        Card.view
            [ defaultCard ]
            [ Card.title [ Options.center ] [ table ]
            , Card.actions [ defaultActions ] actions
            ]

view : (Msg msg -> msg) -> List Int -> Material.Model -> Model -> Html msg
view   lift                idx         mdl               model =
    Html.map lift <| case model.editing of
        True  -> viewEdit idx mdl model
        False -> viewShow idx mdl model

