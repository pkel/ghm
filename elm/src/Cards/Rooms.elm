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

import Form.Show  as FShow
import Form.Parse as FParse


type alias CacheItem =
    { room          : String
    , beds          : String
    , price_per_bed : String
    , factor        : String
    , description   : String
    , from          : String
    , to            : String
    }

emptyCacheItem : CacheItem
emptyCacheItem =
    { room          = ""
    , beds          = ""
    , price_per_bed = ""
    , factor        = ""
    , description   = ""
    , from          = ""
    , to            = ""
    }

initCacheItem : Room -> CacheItem
initCacheItem r =
    let f field = field.set (field.init (field.get r)) in
    emptyCacheItem
    |> f room
    |> f beds
    |> f price_per_bed
    |> f factor
    |> f description
    |> f from
    |> f to

-- room : Input Int
room =
    { set   = \v r -> { r | room = v }
    , get   = .room
    , msg   = Room
    , parse = FParse.int
        |> FParse.check (\x -> x > 0)
        |> FParse.maybe
    , init  =  FShow.maybe  FShow.int
    , hint  = "1, 2, ..."
    , label = "Nr."
    }

beds =
    { set   = \v r -> { r | beds = v }
    , get   = .beds
    , msg   = Beds
    , parse = FParse.int
        |> FParse.check (\x -> x >= 0)
    , init  =  FShow.int
    , hint  = "0, 1, ..."
    , label = "Betten"
    }

price_per_bed =
    { set   = \v r -> { r | price_per_bed = v }
    , get   = .price_per_bed
    , msg   = Price_per_bed
    , parse = FParse.float
    , init  =  FShow.float
    , hint  = toString 44.49
    , label = "Preis"
    }

factor =
    { set   = \v r -> { r | factor = v }
    , get   = .factor
    , msg   = Factor
    , parse = FParse.float
    , init  =  FShow.float
    , hint  = toString 0.75
    , label = "Faktor"
    }

description =
    { set   = \v r -> { r | description = v }
    , get   = .description
    , msg   = Description
    , parse = FParse.string
    , init  =  FShow.string
    , hint  = "Einzelzimmer"
    , label = "Beschreibung"
    }

from =
    { set   = \v r -> { r | from = v }
    , get   = .from
    , msg   = From
    , parse = FParse.maybe FParse.date
    , init  =  FShow.maybe  FShow.date
    , hint  = FShow.dateFormatHint
    , label = "Von"
    }

to =
    { set   = \v r -> { r | to = v }
    , get   = .to
    , msg   = To
    , parse = FParse.maybe FParse.date
    , init  =  FShow.maybe  FShow.date
    , hint  = FShow.dateFormatHint
    , label = "Bis"
    }

extractItem : CacheItem -> Result String Room
extractItem i =
    let mbInt  = FParse.maybe FParse.int
        mbDate = FParse.maybe FParse.date
        ex x = Result.map2 x.set ( x.parse (x.get i))
    in
    Ok Booking.emptyRoom
    |> ex room
    |> ex beds
    |> ex price_per_bed
    |> ex factor
    |> ex description
    |> ex from
    |> ex to

type alias Data = List Room

type alias Model =
    { dirty : Bool
    , cache : Array CacheItem
    , data  : Data
    }

type ItemMsg
    = Room          String
    | Beds          String
    | Price_per_bed String
    | Factor        String
    | Description   String
    | From          String
    | To            String

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


extract : Model -> Result String Data
extract model =
    Array.toList model.cache
    |> List.foldl (\i acc -> Result.map2 (::) (extractItem i) acc) (Ok [])
    |> Result.map List.reverse


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
                    Array.push (initCacheItem Booking.emptyRoom)
            in
            dirty { model | cache = cache_ } |> pure

        Abort ->
            init model.data |> pure

        Save ->
            case extract model of
                Err _ -> pure model
                Ok data -> init data |> callback (cb.updated data)

        Mdl msg -> model |> callback (cb.mdl msg)


updateItem : ItemMsg -> CacheItem -> CacheItem
updateItem msg item =
    let set val field = field.set val item
    in
        case msg of
            Room          str -> set str room
            Beds          str -> set str beds
            Price_per_bed str -> set str price_per_bed
            Factor        str -> set str factor
            Description   str -> set str description
            From          str -> set str from
            To            str -> set str to


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
            let val = spec.get el
                check val = case spec.parse val of
                    Err _ -> False
                    Ok  _ -> True
                error = Textfield.error spec.hint |> Options.when (not <| check val)
                action = Change nth << spec.msg
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

        lst = model.cache |> Array.toIndexedList

        list =
            List.map row lst |> Form.ul

        save = case extract model of
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

