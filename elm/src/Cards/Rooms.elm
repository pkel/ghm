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
    , breakfast     : Bool
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
    , breakfast     = True
    , from          = ""
    , to            = ""
    }

initCacheItem : Room -> CacheItem
initCacheItem x =
    let mbInt  = FShow.maybe FShow.int
        mbDate = FShow.maybe FShow.date
    in
    { room          = x.room          |> mbInt
    , beds          = x.beds          |> FShow.int
    , price_per_bed = x.price_per_bed |> FShow.float
    , factor        = x.factor        |> FShow.float
    , description   = x.description   |> FShow.string
    , breakfast     = x.breakfast
    , from          = x.from          |> mbDate
    , to            = x.to            |> mbDate
    }

extractItem : CacheItem -> Maybe Room
extractItem i =
    let mbInt  = FParse.maybe FParse.int
        mbDate = FParse.maybe FParse.date
        a = Result.map2
    in
    Ok emptyCacheItem
    |> a ( \v r -> { r | room          = v } ) ( mbInt         i.room          )
    |> a ( \v r -> { r | beds          = v } ) ( FParse.int    i.beds          )
    |> a ( \v r -> { r | price_per_bed = v } ) ( FParse.float  i.price_per_bed )
    |> a ( \v r -> { r | factor        = v } ) ( FParse.float  i.factor        )
    |> a ( \v r -> { r | description   = v } ) ( FParse.string i.description   )
    |> a ( \v r -> { r | breakfast     = v } ) ( Ok            i.breakfast     )
    |> a ( \v r -> { r | from          = v } ) ( mbDate        i.from          )
    |> a ( \v r -> { r | to            = v } ) ( mbDate        i.to            )
    |> Result.toMaybe

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
    | Breakfast     Bool

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


extract : Model -> Maybe Data
extract model =
    Array.toList model.cache
    |> List.foldl (\i acc -> Maybe.map2 (::) (extractItem i) acc) (Just [])
    |> Maybe.map List.reverse


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
                Nothing -> pure model
                Just data -> init data |> callback (cb.updated data)

        Mdl msg -> model |> callback (cb.mdl msg)


updateItem : ItemMsg -> CacheItem -> CacheItem
updateItem msg item =
    case msg of
        Room          str -> { item | room          = str  }
        Beds          str -> { item | beds          = str  }
        Price_per_bed str -> { item | price_per_bed = str  }
        Factor        str -> { item | factor        = str  }
        Description   str -> { item | description   = str  }
        From          str -> { item | from          = str  }
        To            str -> { item | to            = str  }
        Breakfast    bool -> { item | breakfast     = bool }


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
                error = Textfield.error hint |> Options.when (not <| check val)
                action = Change nth << up
            in
            Form.textfield Mdl (nth::(id i)) mdl [error] label action val

        miniButton = Defaults.buttonMini Mdl mdl

        button cond = Defaults.button_
            [ Button.disabled |> Options.when (not cond)
            , Button.raised
            ] Mdl mdl

        true str = True

        room          = field 201 "Nr." Room          .room          true ""
        beds          = field 202 ""    Beds          .beds          true ""
        price_per_bed = field 203 "P"   Price_per_bed .price_per_bed true ""
        factor        = field 204 "F"   Factor        .factor        true ""
        description   = field 205 "Dsc" Description   .description   true ""
        from          = field 207 "Frm" From          .from          true ""
        to            = field 208 "To"  To            .to            true ""

        delete (i, _)  = miniButton (i::(id 204)) "delete" (Delete i)

        grid = Form.grid
        cell = Form.cell

        s = Grid.size

        li = Form.li

        form i =
            grid
                [ cell [s All 4] [room   i]
                , cell [s All 4] [beds   i]
                , cell [s All 4] [price_per_bed  i]
                ]

        row i =
            li [ form i, delete i ]

        add = Defaults.button Mdl mdl (id 100) "add" Add

        lst = model.cache |> Array.toIndexedList

        list =
            List.map row lst |> Form.ul |> Form.contain


        actions =
            [ button model.dirty (id 302) "cancel" Abort
            , button model.dirty (id 303) "save"   Save
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

