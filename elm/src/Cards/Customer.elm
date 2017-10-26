module Cards.Customer exposing
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

import Customer exposing (Customer)

import Html exposing (Html, text, br)
import Html.Attributes as Attributes

type alias Model =
    { history : List Modifier
    , latest  : Modifier
    , tab     : Int
    , buffer  : Input.Buffer
    , data    : Customer
    }

stringSpec : String -> String -> Input.Spec String
stringSpec key label =
    { key = key
    , typeSpec = Input.string
    , hint  = ""
    , label = label
    }

keyword         = stringSpec "keyword"         "Kürzel"
title           = stringSpec "title"           "Anrede"
title_letter    = stringSpec "title_letter"    "Anrede Brief"
given           = stringSpec "given"           "Vorname"
second          = stringSpec "second"          "Zweitname"
family          = stringSpec "family"          "Nachname"
street          = stringSpec "street"          "Straße"
street_number   = stringSpec "street_number"   "Hnr."
postal_code     = stringSpec "postal_code"     "Postleitzahl"
city            = stringSpec "city"            "Ort"
country         = stringSpec "country"         "Land"
country_code    = stringSpec "country_code"    "Ländercode"
company         = stringSpec "company"         "Firma"
company_address = stringSpec "company_address" "Adresse Firma"
phone           = stringSpec "phone"           "Telefon"
phone2          = stringSpec "phone2"          "Telefon"
mobile          = stringSpec "mobile"          "Mobiltelefon"
fax             = stringSpec "fax"             "Fax"
fax2            = stringSpec "fax2"            "Fax"
mail            = stringSpec "mail"            "Email"
mail2           = stringSpec "mail2"           "Email"
web             = stringSpec "web"             "Website"

type Msg msg
    = Change Input.Updater String
    | Undo
    | SelectTab Int
    | Mdl (Material.Msg msg)

type alias Modifier = Customer -> Customer

type alias Callbacks msg =
    { updated : (Modifier) -> msg
    , mdl     : Material.Msg msg -> msg
    }

id : Modifier
id m = m

init : Customer -> Model
init c =
    { history = []
    , latest = id
    , tab    = 0
    , buffer = initBuffer c
    , data   = c
    }

initBuffer : Customer -> Input.Buffer
initBuffer c =
    let f spec get = Input.init spec (get c)
    in
        Input.empty
        |> f title           .title
        |> f title_letter    .title_letter
        |> f given           .given
        |> f second          .second
        |> f family          .family
        |> f company         .company
        |> f company_address .company_address
        |> f street          .street
        |> f street_number   .street_number
        |> f city            .city
        |> f postal_code     .postal_code
        |> f country         .country
        |> f country_code    .country_code
        |> f phone           .phone
        |> f phone2          .phone2
        |> f mobile          .mobile
        |> f fax             .fax
        |> f fax2            .fax2
        |> f mail            .mail
        |> f mail2           .mail2
        |> f web             .web
        |> f keyword         .keyword


parse : Model -> Result String Modifier
parse model =
    let buf = model.buffer
        mod set str acc m = (set str) (acc m)
        f spec set = Result.map2 (mod set) (Input.parse spec buf)
    in
        Ok (\x -> x)
        |> f title           (\v r -> { r | title           = v } )
        |> f title_letter    (\v r -> { r | title_letter    = v } )
        |> f given           (\v r -> { r | given           = v } )
        |> f second          (\v r -> { r | second          = v } )
        |> f family          (\v r -> { r | family          = v } )
        |> f company         (\v r -> { r | company         = v } )
        |> f company_address (\v r -> { r | company_address = v } )
        |> f street          (\v r -> { r | street          = v } )
        |> f street_number   (\v r -> { r | street_number   = v } )
        |> f city            (\v r -> { r | city            = v } )
        |> f postal_code     (\v r -> { r | postal_code     = v } )
        |> f country         (\v r -> { r | country         = v } )
        |> f country_code    (\v r -> { r | country_code    = v } )
        |> f phone           (\v r -> { r | phone           = v } )
        |> f phone2          (\v r -> { r | phone2          = v } )
        |> f mobile          (\v r -> { r | mobile          = v } )
        |> f fax             (\v r -> { r | fax             = v } )
        |> f fax2            (\v r -> { r | fax2            = v } )
        |> f mail            (\v r -> { r | mail            = v } )
        |> f mail2           (\v r -> { r | mail2           = v } )
        |> f web             (\v r -> { r | web             = v } )
        |> f keyword         (\v r -> { r | keyword         = v } )



update : UpdateCallback msg (Callbacks msg) (Msg msg) Model
update cb msg model =
    case msg of
        Change up str ->
          { model | buffer = Input.update up str model.buffer }
          |> \m -> case parse m of
            Err _ -> pure m
            Ok  mod ->
              { m
              | history = model.latest :: model.history
              , latest = mod
              }
              |> callback (cb.updated mod)

        Undo ->
          case model.history of
            [] -> pure model
            hd :: tl ->
              { model
              | latest = hd
              , buffer = hd model.data |> initBuffer
              , history = tl
              }
              |> callback (cb.updated hd)

        SelectTab i ->
            pure { model | tab = i }

        Mdl msg -> callback (cb.mdl msg) model

-- View

type alias Cfg msg =
    { index      : List Int
    , lift       : Msg msg -> msg
    , title      : Maybe String
    }

view : Cfg msg -> Material.Model -> Model -> Html msg
view cfg mdl model =
    let index x = (x :: cfg.index)

        button cond i = Defaults.button_
            [ Button.disabled |> Options.when (not cond)
            , Button.raised
            ] Mdl mdl (index i)

        dirty = case model.history of
          [] -> False
          _ -> True


        actions = [ button dirty 1 "undo" Undo
                  ]

        tab_labels =
            [ "Name", "Adresse", "Kontakt" ]
            |> List.map (\l ->
                    Tabs.textLabel
                        [ Options.center
                        , Options.css "cursor" "pointer"
                        ] l )

        tf i spec data =
            Form.textfield Mdl (index i) mdl [] spec.label data
                (Change (Input.updater spec)) (Input.get spec model.buffer)

        s        = Grid.size
        full     = [ s Desktop 12, s Tablet 8, s Phone 4 ]
        one4th   = [ s Desktop 3 , s Tablet 2, s Phone 1 ]
        three4th = [ s Desktop 9 , s Tablet 6, s Phone 3 ]
        half     = [ s Desktop 6 , s Tablet 4, s Phone 2 ]

        f size i spec data =
            Form.cell size [ tf i spec data ]

        grid = Form.grid

        titleOp = Just ["Herr", "Frau"]
        titleLOp = Just
            [ "Sehr geehrter Herr"
            , "Sehr geehrte Frau"
            ]

        nameTab =
            grid
                [ f full     5  keyword      Nothing
                , f one4th   6  title        titleOp
                , f three4th 7  title_letter titleLOp
                , f half     8  given        Nothing
                , f half     9  second       Nothing
                , f full     10 family       Nothing
                ]
            |> Form.contain

        addressTab =
            grid
                [ f three4th 21 street          Nothing
                , f one4th   22 street_number   Nothing
                , f one4th   24 postal_code     Nothing
                , f three4th 23 city            Nothing
                , f three4th 25 country         Nothing
                , f one4th   26 country_code    Nothing
                , f full     27 company         Nothing
                , f full     28 company_address Nothing
                ]
            |> Form.contain

        contactTab =
            grid
                [ f half 41 phone  Nothing
                , f half 42 phone2 Nothing
                , f full 43 mobile Nothing
                , f half 44 fax    Nothing
                , f half 45 fax2   Nothing
                , f full 46 mail   Nothing
                , f full 47 mail2  Nothing
                , f full 48 web    Nothing
                ]
            |> Form.contain

        tabs = Tabs.render Mdl (index 4) mdl
                [ Tabs.onSelectTab SelectTab
                , Tabs.activeTab model.tab
                ]
                tab_labels
                [ case model.tab of
                    1 -> addressTab
                    2 -> contactTab
                    _ -> nameTab
                ]
    in
        [ Card.actions [] [ tabs ]
        , Card.actions [ Defaults.actions ] actions
        ]
        |> ( \x -> case cfg.title of
            Nothing -> x
            Just title ->
                Card.title [ Defaults.cardTitle ] [ text title ] :: x )
        |> Card.view [ Defaults.card ]
        |> Html.map cfg.lift

