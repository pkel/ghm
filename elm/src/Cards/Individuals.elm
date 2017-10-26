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
import Material.Style as Style
import Material.GriddedForm as Form

import Booking exposing (Individual)

import Defaults

import Html exposing (Html, text)
import Html.Attributes as Attributes

import Helpers.Array as ArrayX
import Array exposing (Array)

import Date exposing (Date)

import BufferedInput as Input

given : Input.Spec String
given =
    { key      = "given"
    , hint     = ""
    , label    = "Vorname"
    , typeSpec = Input.string
    }

family : Input.Spec String
family =
    { key      = "family"
    , hint     = ""
    , label    = "Nachname"
    , typeSpec = Input.string
    }

birth : Input.Spec (Maybe Date)
birth =
    { key      = "birth"
    , hint     = Input.dateFormatHint
    , label    = "Geburtsdatum"
    , typeSpec = Input.maybe Input.date
    }

initBuffer : Individual -> Input.Buffer
initBuffer i =
    let f spec get = Input.init spec (get i)
    in
        Input.empty
        |> f given  .given
        |> f family .family
        |> f birth  .date_of_birth

parseIndividual : Input.Buffer -> Result String Individual
parseIndividual buf =
    let f spec set = Result.map2 set (Input.parse spec buf)
    in
        -- TODO: This potentially deletes field which where there on init
        -- compare Cards.Customer
        Ok Booking.emptyIndividual
        |> f given  (\v r -> { r | given  = v})
        |> f family (\v r -> { r | family = v})
        |> f birth  (\v r -> { r | date_of_birth  = v})

type alias Data = List Individual

type alias Model =
    { dirty  : Bool
    , buffer : Array Input.Buffer
    , data   : Data
    }

type Msg msg
    = Change Int Input.Updater String
    | Delete Int
    | Add
    | Abort
    | Save
    | Mdl (Material.Msg msg)

init : Data -> Model
init data =
    { buffer = Array.fromList (List.map initBuffer data)
    , data  = data
    , dirty = False
    }

dirty : Model -> Model
dirty model =
    { model | dirty = True }

parse : Model -> Result String Data
parse model =
    Array.toList model.buffer
    |> List.foldl (\i acc -> Result.map2 (::) (parseIndividual i) acc) (Ok [])
    |> Result.map List.reverse


-- Update

type alias Callbacks msg =
    { updated : Data -> msg
    , mdl     : Material.Msg msg -> msg
    }

update : UpdateCallback msg (Callbacks msg) (Msg msg) Model
update cb msg model =
    case msg of
        Change index updater str ->
            let f = Input.update updater str
                buffer_ = ArrayX.modify index f model.buffer
            in
            dirty { model | buffer = buffer_ } |> pure

        Delete index ->
            let buffer_ =
                    ArrayX.delete index model.buffer
            in
            dirty { model | buffer = buffer_ } |> pure

        Add ->
            let buffer_ =
                    model.buffer |>
                    Array.push (initBuffer Booking.emptyIndividual)
            in
            dirty { model | buffer = buffer_ } |> pure

        Abort ->
            init model.data |> pure

        Save ->
            case parse model of
                Err _ -> pure model
                Ok data -> init data |> callback (cb.updated data)

        Mdl msg -> model |> callback (cb.mdl msg)


-- View

type alias Cfg msg =
    { index : List Int
    , lift : Msg msg -> msg
    , title : Maybe String
    }

view : Cfg msg -> Material.Model -> Model -> Html msg
view cfg mdl model =
    let id x = (x :: cfg.index)

        -- TODO: This might belong into a seperate module
        field i spec (nth,el) =
            let val = Input.get spec el
                error = Textfield.error spec.hint
                    |> Options.when (not <| Input.valid spec el)
                action str = Change nth (Input.updater spec) str
            in
            Form.textfield Mdl (nth::(id i)) mdl [error]
                spec.label Nothing action val

        miniButton = Defaults.buttonMini Mdl mdl

        button cond = Defaults.button_
            [ Button.disabled |> Options.when (not cond)
            , Button.raised
            ] Mdl mdl

        f_given  = field 201 given
        f_family = field 202 family
        f_birth  = field 203 birth

        delete (i, _)  = miniButton (i::(id 204)) "delete" (Delete i)

        grid = Form.grid
        cell = Form.cell

        s = Grid.size

        li = Form.li

        form i =
            grid
                [ cell [s All 4] [f_given  i]
                , cell [s All 4] [f_family i]
                , cell [s All 4] [f_birth  i]
                ]

        row i =
            li [ form i, delete i ]

        add = Defaults.button Mdl mdl (id 100) "add" Add

        lst = model.buffer |> Array.toIndexedList

        list =
            List.map row lst |> Form.ul |> Form.contain

        save = case parse model of
            Err _ -> False
            Ok _  -> model.dirty

        actions =
            [ button model.dirty (id 302) "cancel" Abort
            , button save        (id 303) "save"   Save
            , button True        (id 304) "add"    Add
            ]
    in
        [ Card.actions [] [ list ]
        , Card.actions [ Defaults.actions ] actions
        ]
        |> ( \x -> case cfg.title of
            Nothing -> x
            Just title ->
                Card.title [ Defaults.cardTitle ] [ text title ] :: x )
        |> Card.view [ Defaults.card ]
        |> Html.map cfg.lift

