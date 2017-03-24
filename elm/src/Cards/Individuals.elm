module Cards.BookedIndividuals exposing
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

import Booking exposing (BookedIndividual)

import Defaults exposing (..)

import Html exposing (Html, text)
import Html.Attributes as Attributes

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

initCacheItem : BookedIndividual -> CacheItem
initCacheItem x =
    { given         = x.given
    , family        = x.family
    -- TODO: day of birth editing
    , date_of_birth = ""
    }

type alias Model =
    { editMode : Bool
    , focus : Int
    , cache : IndexedList CacheItem
    }

type alias Cfg msg =
    { mdl        : Material.Model
    , mdlMessage : (Material.Msg msg -> msg)
    , msg        : Msg -> msg
    , index      : List Int
    , title      : String
    , edit       : msg
    , done       : msg
    }

type ItemMsg
    = Given         String
    | Family        String
    | Date_of_birth String

type Msg
    = CacheItemChange Int ItemMsg
    | CacheItemDelete Int
    | CacheItemAdd
    | Abort

show : Model
show =
    { editMode = False
    , focus = 0
    , cache = []
    }

edit : List BookedIndividual -> Model
edit lst =
    { editMode = True
    , focus = IndexedList.firstIndex
    , cache = IndexedList.fromList (List.map initCacheItem lst)
    }

-- TODO: This should check the data for errors
extract : Model -> Maybe String
extract model =
    case model.editMode of
        True -> Just (IndexedList.toList model.cache)
        False -> Nothing

updateItem : ItemMsg -> BookedIndividual -> BookedIndividual
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
        CacheItemChange index itemMsg ->
            let cache_ =
                    IndexedList.updateAt index updateItem itemMsg
            in
            ( { model | cache = cache_ }, Cmd.none )

        CacheItemDelete index ->
            let cache_ =
                    IndexedList.delete index
            in
            ( { model | cache = cache_ }, Cmd.none )

        CacheItemAdd ->
            let (focus_, cache_) =
                    IndexedList.add BookedIndividual.empty
            in
            ( { model | cache = cache_, focus = focus_ }, Cmd.none )

        Abort ->
            ( show, Cmd.none )

viewEdit : Cfg msg -> Model -> Html msg
viewEdit cfg model =
    viewShow cfg (IndexedList.toList model.cache)

viewShow : Cfg msg -> List BookedIndividual -> Html msg
viewShow cfg lst =
    let birth i = text
            ( Maybe.withDefault "n/a"
                ( Maybe.map (DateF.format "%d.%m.%Y") i.date_of_birth)
            )

        given i = text i.given
        family i = text i.family

        defaultButton_ = defaultButton cfg.mdlMessage cfg.mdl
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
            [ defaultButton_ (i 102) "mode_edit" cfg.edit
            ]
    in
        Card.view
            [ defaultCard ]
            [ Card.title [ defaultCardTitle ] [ text cfg.title ]
            , Card.title [ Options.center ] [ table ]
            , Card.actions [ defaultActions ] actions
            ]

view : Cfg msg -> Model -> List BookedIndividual -> Html msg
view cfg model lst =
    case model.editMode of
        True -> viewEdit cfg model
        False -> viewShow cfg lst

