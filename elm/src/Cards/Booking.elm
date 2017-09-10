module Cards.Booking exposing
    ( Model
    , Cfg
    , Callbacks
    , Msg
    , view
    , init
    , update
    )

import Material
import Material.Helpers  exposing (pure, effect)
import Material.HelpersX exposing (callback, UpdateCallback)

import Material.Card as Card
import Material.Grid as Grid exposing (Device(..))
import Material.Tabs as Tabs
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Style as Style
import Material.GriddedForm as Form

import Defaults

import BufferedInput as Input

import Booking exposing (Booking)

import Html exposing (Html, text, br)
import Html.Attributes as Attributes

type alias Model =
    { dirty  : Bool
    , buffer : Input.Buffer
    , data   : Booking
    }

deposit_asked : Input.Spec (Maybe Float)
deposit_asked =
    { key      = "deposit_asked"
    , label    = "Anzahlung gefordert"
    , hint     = "30.20"
    , typeSpec = Input.float
        |> Input.check (\x -> x >= 0.0 )
        |> Input.maybe
    }

deposit_got : Input.Spec (Maybe Float)
deposit_got =
    { key      = "deposit_got"
    , label    = "Anzahlung erhalten"
    , hint     = "30.20"
    , typeSpec = Input.float
        |> Input.check (\x -> x >= 0.0)
        |> Input.maybe
    }

type Msg msg
    = Change Input.Updater String
    | Abort
    | Delete
    | Save
    | Mdl (Material.Msg msg)

type alias Callbacks msg =
    { delete  : msg
    , updated : (Booking -> Booking) -> msg
    , mdl     : Material.Msg msg -> msg
    }

init : Booking -> Model
init x =
    { dirty  = False
    , buffer = initBuffer x
    , data   = x
    }

dirty : Model -> Model
dirty model =
    { model | dirty = True }

initBuffer : Booking -> Input.Buffer
initBuffer x =
    let f spec get = Input.init spec (get x)
    in
        Input.empty
        |> f deposit_got   .deposit_got
        |> f deposit_asked .deposit_asked


parse : Model -> Result String (Booking -> Booking)
parse model =
    let buf = model.buffer
        f spec set = Result.map2 set (Input.parse spec buf)
    in
        -- TODO: Advanced uglyness
        -- The intention is to avoid the overwriting of
        -- booking.rooms/individuals and only set the values, this card cares of
        Ok Booking.empty
        |> f deposit_asked   (\v r -> { r | deposit_asked   = v } )
        |> f deposit_got     (\v r -> { r | deposit_got     = v } )
        |> Result.map (\new was ->
            { was
            | deposit_got   = new.deposit_got
            , deposit_asked = new.deposit_asked
            } )


update : UpdateCallback msg (Callbacks msg) (Msg msg) Model
update cb msg model =
    case msg of
        Change up str ->
            dirty { model | buffer = Input.update up str model.buffer } |> pure

        Abort ->
            { model | buffer = initBuffer model.data, dirty = False } |> pure

        Save -> case parse model of
            Err _ -> pure model
            Ok mod -> { model | data = mod model.data, dirty = False }
                |> callback (cb.updated mod)

        Delete -> callback cb.delete model

        Mdl msg -> callback (cb.mdl msg) model

-- View

type alias Cfg msg =
    { index      : List Int
    , lift       : Msg msg -> msg
    }

view : Cfg msg -> Material.Model -> Model -> Html msg
view cfg mdl model =
    let index x = (x :: cfg.index)

        button cond i = Defaults.button_
            [ Button.disabled |> Options.when (not cond)
            , Button.raised
            ] Mdl mdl (index i)

        save = case parse model of
            Err _ -> False
            Ok _  -> model.dirty

        actions = [ button model.dirty 1 "cancel" Abort
                  , button save        2 "save"   Save
                  ]

        tf i spec =
            let val = Input.get spec model.buffer
                error = Textfield.error spec.hint
                    |> Options.when (not <| Input.valid spec model.buffer)
                action str = Change (Input.updater spec) str
            in
            Form.textfield Mdl (index i) mdl [error] spec.label action val

        s        = Grid.size
        full     = [ s Desktop 12, s Tablet 8, s Phone 4 ]
        one4th   = [ s Desktop 3 , s Tablet 2, s Phone 1 ]
        three4th = [ s Desktop 9 , s Tablet 6, s Phone 3 ]
        half     = [ s Desktop 6 , s Tablet 4, s Phone 2 ]

        f size i spec =
            Form.cell size [ tf i spec ]

        grid = Form.grid

        body =
            grid
                [ f half     8  deposit_asked
                , f half     9  deposit_got
                ]
            |> Form.contain

        cardContent =
            -- [ Card.title [ Defaults.cardTitle ] [ text model.cache.keyword ]
            [ Card.actions [] [ body ]
            , Card.actions [ Defaults.actions ] actions
            ]
    in
        Card.view [ Defaults.card ] cardContent
        |> Html.map cfg.lift

