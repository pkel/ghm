module Cards.Individuals exposing
    ( Model
    , Msg
    , Cfg
    , Callbacks
    , view
    , init
    , update
    )

import Material
import Material.Helpers exposing (pure, effect)
import Material.HelpersX exposing (callback, UpdateCallback)
import Material.Card as Card
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Grid as Grid exposing (Device(..))
import Material.List as Lists
import Material.Button as Button

import Booking exposing (Individual)

import Defaults

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

type alias Model =
    { dirty : Bool
    , cache : Array CacheItem
    , data  : Data
    }

type ItemMsg
    = Given  String
    | Family String
    | Birth  String

type Msg msg
    = Change Int ItemMsg
    | Delete Int
    | Add
    | Abort
    | Save
    | Mdl (Material.Msg msg)

init : Data -> Model
init data =
    { cache = Array.fromList (List.map initCacheItem data)
    , data  = data
    , dirty = False
    }

dirty : Model -> Model
dirty model =
    { model | dirty = True }

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

type alias Callbacks msg =
    { updated : Data -> msg
    , mdl     : Material.Msg msg -> msg
    }

update : UpdateCallback msg (Callbacks msg) (Msg msg) Model
update cb msg model =
    case msg of
        Change index itemMsg ->
            let cache_ =
                    case Array.get index model.cache of
                        Nothing -> model.cache
                        Just el -> updateItem itemMsg el
                            |> \x -> Array.set index x model.cache
            in
            dirty { model | cache = cache_ } |> pure

        Delete index ->
            let cache_ =
                    ArrayX.delete index model.cache
            in
            dirty { model | cache = cache_ } |> pure

        Add ->
            let cache_ =
                    model.cache |>
                    Array.push (initCacheItem Booking.emptyIndividual)
            in
            dirty { model | cache = cache_ } |> pure

        Abort ->
            init model.data |> pure

        Save ->
            case extract model of
                Nothing -> pure model
                Just data -> init data |> callback (cb.updated data)

        Mdl msg -> model |> callback (cb.mdl msg)


updateItem : ItemMsg -> CacheItem -> CacheItem
updateItem msg item =
    case msg of
        Given str ->
            { item | given = str }
        Family str ->
            { item | family = str }
        Birth str ->
            { item | birth = str }


-- View

type alias Cfg msg =
    { index : List Int
    , lift : Msg msg -> msg
    , title : Maybe String
    }

view : Cfg msg -> Material.Model -> Model -> Html msg
view cfg mdl model =
    let id x = (x :: cfg.index)

        field i label up show check hint (nth,el) =
            let val = show el
                props =
                [ Options.onInput (\x -> Change nth (up x))
                , Textfield.label label
                , Textfield.floatingLabel
                , Textfield.value val
                , Textfield.error hint |> Options.when (not <| check val)
                , Options.css "width" "100%"
                -- , Options.css "padding-bottom" "1"
                -- , Options.css "font-size" "13px"
                ]
            in
            Textfield.render Mdl (nth::(id i)) mdl
                props []

        miniButton = Defaults.buttonMini Mdl mdl

        button cond = Defaults.button_
            [ Button.disabled |> Options.when (not cond)
            , Button.raised
            ] Mdl mdl

        true str = True

        checkBirth str = case extractBirth str of
            Ok _  -> True
            Err _ -> False

        given  = field 201 "Vorname"    Given  .given  true       ""
        family = field 202 "Nachname"   Family .family true       ""
        birth  = field 203 "Geburtstag" Birth  .birth  checkBirth dateFormatHint

        delete (i, _)  = miniButton (i::(id 204)) "delete" (Delete i)

        p0 = Options.css "padding" "0"
        w100 = Options.css "width" "100%"

        grid = Grid.grid [ p0, w100 ]
        cell = Grid.cell
        s = Grid.size
        li = Lists.li [ p0 ]

        form i =
            grid
                [ cell [s All 4] [given  i]
                , cell [s All 4] [family i]
                , cell [s All 4] [birth  i]
                ]

        row i =
            li
                [ Lists.content [] [ form i ]
                , delete i
                ]

        add = Defaults.button Mdl mdl (id 100) "add" Add

        lst = model.cache |> Array.toIndexedList

        list =
            List.map row lst |> Lists.ul [ p0 ]


        actions =
            [ button model.dirty (id 302) "cancel" Abort
            , button model.dirty (id 303) "save"   Save
            , button True        (id 304) "add"    Add
            ]
    in
        [ Card.text [] [ list ]
        , Card.actions [ Defaults.actions ] actions
        ]
        |> ( \x -> case cfg.title of
            Nothing -> x
            Just title ->
                Card.title [ Defaults.cardTitle ] [ text title ] :: x )
        |> Card.view [ Defaults.card ]
        |> Html.map cfg.lift

