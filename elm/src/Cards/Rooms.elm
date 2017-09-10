module Cards.Rooms exposing
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

import Booking exposing (Room)

import Defaults

import Html exposing (Html, text)
import Html.Attributes as Attributes

import Array exposing (Array)
import Helpers.Array as ArrayX

import BufferedInput as Input

import Date exposing (Date)

room : Input.Spec (Maybe Int)
room =
    { key   = "room"
    , hint  = "1, 2, ..."
    , label = "Nr."
    , typeSpec = Input.int
        |> Input.check (\x -> x > 0)
        |> Input.maybe
    }

beds : Input.Spec Int
beds =
    { hint  = "0, 1, ..."
    , label = "Betten"
    , key   = "beds"
    , typeSpec = Input.int
        |> Input.check (\x -> x >= 0)
    }

price_per_bed : Input.Spec Float
price_per_bed =
    { hint  = toString 44.49
    , label = "Preis"
    , typeSpec = Input.float
    , key   = "price"
    }

factor : Input.Spec Float
factor =
    { hint  = toString 0.75
    , label = "Faktor"
    , key   = "factor"
    , typeSpec = Input.float
    }

description : Input.Spec String
description =
    { hint  = "Einzelzimmer"
    , label = "Beschreibung"
    , key   = "description"
    , typeSpec = Input.string
    }

from : Input.Spec (Maybe Date)
from =
    { hint  = Input.dateFormatHint
    , label = "Von"
    , key   = "from"
    , typeSpec = Input.maybe Input.date
    }

to : Input.Spec (Maybe Date)
to =
    { hint  = Input.dateFormatHint
    , label = "Bis"
    , key   = "to"
    , typeSpec = Input.maybe Input.date
    }

initBuffer : Room -> Input.Buffer
initBuffer r =
    let f spec get = Input.init spec (get r) in
    Input.empty
    |> f room          .room
    |> f beds          .beds
    |> f price_per_bed .price_per_bed
    |> f factor        .factor
    |> f description   .description
    |> f from          .from
    |> f to            .to

parseRoom : Input.Buffer -> Result String Room
parseRoom buf =
    let f spec set = Result.map2 set (Input.parse spec buf)
    in
    Ok Booking.emptyRoom
    |> f room          (\v r -> { r | room          = v } )
    |> f beds          (\v r -> { r | beds          = v } )
    |> f price_per_bed (\v r -> { r | price_per_bed = v } )
    |> f factor        (\v r -> { r | factor        = v } )
    |> f description   (\v r -> { r | description   = v } )
    |> f from          (\v r -> { r | from          = v } )
    |> f to            (\v r -> { r | to            = v } )


type alias Data = List Room

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
    |> List.foldl (\i acc -> Result.map2 (::) (parseRoom i) acc) (Ok [])
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
                    Array.push (initBuffer Booking.emptyRoom)
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

        field i spec (nth,el) =
            let val = Input.get spec el
                error = Textfield.error spec.hint
                    |> Options.when (not <| Input.valid spec el)
                action str = Change nth (Input.updater spec) str
            in
            Form.textfield Mdl (nth::(id i)) mdl [error] spec.label action val

        miniButton = Defaults.buttonMini Mdl mdl

        button cond = Defaults.button_
            [ Button.disabled |> Options.when (not cond)
            , Button.raised
            ] Mdl mdl

        true str = True

        f_room          = field 201 room
        f_beds          = field 202 beds
        f_price_per_bed = field 203 price_per_bed
        f_factor        = field 204 factor
        f_description   = field 205 description
        f_from          = field 207 from
        f_to            = field 208 to

        delete (i, _)  = miniButton (i::(id 204)) "delete" (Delete i)

        grid = Form.grid
        cell = Form.cell

        s = Grid.size

        li = Form.li

        form i =
            grid
                [ cell [s All 2]  [f_room   i]
                , cell [s All 2]  [f_beds   i]
                , cell [s All 2]  [f_price_per_bed  i]
                , cell [s All 2]  [f_factor  i]
                , cell [s All 4]  [f_from  i]
                , cell [s All 8]  [f_description  i]
                , cell [s All 4]  [f_to  i]
                ]

        row i =
            li [ form i |> Form.contain , delete i ]

        add = Defaults.button Mdl mdl (id 100) "add" Add

        lst = model.buffer |> Array.toIndexedList

        list =
            List.map row lst |> Form.ul

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

