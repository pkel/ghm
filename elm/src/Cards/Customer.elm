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
import Material.HelpersX exposing (callback)

import Material.Card as Card
import Material.Grid as Grid exposing (Device(..))
import Material.Tabs as Tabs
import Material.Textfield as Textfield
import Material.Options as Options

import Defaults exposing (..)

import Customer exposing (Customer)

import Html exposing (Html, text, br)
import Html.Attributes as Attributes

type alias Model =
    { editing : Bool
    , editTab : Int
    , cache : Customer
    , data  : Customer
    }

type alias Cfg msg =
    { index      : List Int
    , lift       : Msg msg -> msg
    }

type ChangeMsg
    = Title           String
    | Title_letter    String

    | Given           String
    | Second          String
    | Family          String

    | Company         String
    | Company_address String

    | Street          String
    | Street_number   String
    | City            String
    | Postal_code     String
    | Country         String
    | Country_code    String

    | Phone1          String -- Naming conflict with Material.Grid Device Phone
    | Phone2          String
    | Mobile          String
    | Fax             String
    | Fax2            String
    | Mail            String
    | Mail2           String
    | Web             String

    | Keyword         String


type Msg msg
    = Change ChangeMsg
    | Edit
    | Abort
    | Delete
    | Done
    | SelectTab Int
    | Mdl (Material.Msg msg)

type alias Callbacks msg =
    { delete  : msg
    , updated : Customer -> msg
    , mdl     : Material.Msg msg -> msg
    }

init : Customer -> Model
init c =
    { editing = False
    , editTab = 0
    , cache   = Customer.empty
    , data    = c
    }

edit : Model -> Model
edit model =
    { model
    | editing = True
    , editTab = 0
    , cache = model.data
    }

-- Update given Customer with editable fields from
-- cache TODO: if form is ok.
extract : Model -> Maybe Customer
extract model =
    let c = model.cache
        was = model.data
    in
        Just
            { was
            | title           = c.title
            , title_letter    = c.title_letter

            , given           = c.given
            , second          = c.second
            , family          = c.family

            , company         = c.company
            , company_address = c.company_address

            , street          = c.street
            , street_number   = c.street_number
            , city            = c.city
            , postal_code     = c.postal_code
            , country         = c.country
            , country_code    = c.country_code

            , phone           = c.phone
            , phone2          = c.phone2
            , mobile          = c.mobile
            , fax             = c.fax
            , fax2            = c.fax2
            , mail            = c.mail
            , mail2           = c.mail2
            , web             = c.web

            , keyword         = c.keyword
            }


update : Callbacks msg -> Msg msg -> Model -> ( Model, Cmd msg )
update cb msg model =
    case msg of
        Change msg ->
            pure { model | cache = updateCache msg model.cache }

        Abort ->
            pure { model | editing = False }

        SelectTab i ->
            pure { model | editTab = i }

        Edit ->
            edit model |> pure

        Done -> case extract model of
            Nothing -> pure model
            Just c -> init c |> callback (cb.updated c)

        Delete -> callback cb.delete model

        Mdl msg -> callback (cb.mdl msg) model

updateCache : ChangeMsg -> Customer -> Customer
updateCache msg c =
    case msg of
        Title           s -> { c | title           = s }
        Title_letter    s -> { c | title_letter    = s }

        Given           s -> { c | given           = s }
        Second          s -> { c | second          = s }
        Family          s -> { c | family          = s }

        Company         s -> { c | company         = s }
        Company_address s -> { c | company_address = s }

        Street          s -> { c | street          = s }
        Street_number   s -> { c | street_number   = s }
        City            s -> { c | city            = s }
        Postal_code     s -> { c | postal_code     = s }
        Country         s -> { c | country         = s }
        Country_code    s -> { c | country_code    = s }

        Phone1          s -> { c | phone           = s }
        Phone2          s -> { c | phone2          = s }
        Mobile          s -> { c | mobile          = s }
        Fax             s -> { c | fax             = s }
        Fax2            s -> { c | fax2            = s }
        Mail            s -> { c | mail            = s }
        Mail2           s -> { c | mail2           = s }
        Web             s -> { c | web             = s }

        Keyword         s -> { c | keyword         = s }


viewEdit : Cfg msg -> Material.Model -> Model -> Html (Msg msg)
viewEdit cfg mdl model =
    let index x = (x :: cfg.index)

        defaultButton_ i = defaultButton Mdl mdl (index i)

        actions = [ defaultButton_ 1 "done"   Done
                  , defaultButton_ 2 "cancel" Abort
                  , defaultButton_ 3 "delete" Delete
                  ]

        tab_labels =
            [ "Name", "Adresse", "Kontakt" ]
            |> List.map (\l ->
                    Tabs.textLabel
                        [ Options.center
                        , Options.css "cursor" "pointer"
                        ] l )

        tf i label value msg =
            Textfield.render Mdl (index i) mdl
                [ Textfield.value (value model.cache)
                , Textfield.floatingLabel
                , Textfield.label (label)
                , Textfield.text_
                , Options.css "width" "100%"
                , Options.onInput (Change << msg)
                ] ()

        s        = Grid.size
        full     = [ s Desktop 12, s Tablet 8, s Phone 4 ]
        one4th   = [ s Desktop 3 , s Tablet 2, s Phone 1 ]
        three4th = [ s Desktop 9 , s Tablet 6, s Phone 3 ]
        half     = [ s Desktop 6 , s Tablet 4, s Phone 2 ]

        f size i label value msg = Grid.cell size [ tf i label value msg ]

        grid = Grid.grid [ Options.css "padding" "0" ]

        nameTab =
            grid
                [ f full     5  "Kürzel"       .keyword      Keyword
                , f one4th   6  "Anrede"       .title        Title
                , f three4th 7  "Anrede Brief" .title_letter Title_letter
                , f half     8  "Vorname"      .given        Given
                , f half     9  "Zweitname"    .second       Second
                , f full     10 "Nachname"     .family       Family
                ]

        addressTab =
            grid
                [ f three4th 21 "Straße"       .street        Street
                , f one4th   22 "Hnr."         .street_number Street_number
                , f one4th   24 "Postleitzahl" .postal_code   Postal_code
                , f three4th 23 "Ort"          .city          City
                , f three4th 25 "Land"         .country       Country
                , f one4th   26 "Ländercode"   .country_code  Country_code
                ]

        contactTab =
            grid
                [ f half 41 "Telefon"      .phone  Phone1
                , f half 42 "Telefon"      .phone2 Phone2
                , f full 43 "Mobiltelefon" .mobile Mobile
                , f half 44 "Fax"          .fax    Fax
                , f half 45 "Fax"          .fax2   Fax2
                , f full 46 "Email"        .mail   Mail
                , f full 47 "Email"        .mail2  Mail2
                , f full 48 "Website"      .web    Web
                ]

        tabs = Tabs.render Mdl (index 4) mdl
                [ Tabs.onSelectTab SelectTab
                , Tabs.activeTab model.editTab
                ]
                tab_labels
                [ case model.editTab of
                    1 -> addressTab
                    2 -> contactTab
                    _ -> nameTab
                ]

        cardContent =
            [ Card.title [ defaultCardTitle ] [ text model.cache.keyword ]
            , Card.text [] [ tabs ]
            , Card.actions [ defaultActions ] actions
            ]
    in
        Card.view [ defaultCard ] cardContent


viewShow : Cfg msg -> Material.Model -> Model -> Html (Msg msg)
viewShow cfg mdl model =
    let i x = (x :: cfg.index)

        defaultButton_ x = defaultButton Mdl mdl (i x)

        actions =
            [ defaultButton_ 1 "mode_edit" Edit
            , defaultButton_ 2 "delete"    Delete
            ]

        -- TODO: export to Extra.List/String
        nonEmpty str =
            case String.trim str of
                "" -> Nothing
                str -> Just str

        joinNonEmpty sep lst =
            List.filterMap nonEmpty lst
                |> String.join sep

        append post pre =
            pre ++ post

        appendIfNotEmpty check post pre =
            case nonEmpty check of
                Nothing -> pre
                Just _  -> append post pre

        f str =
            appendIfNotEmpty str [ text str, br [] [] ]

        g = joinNonEmpty

        c = model.data

        main_ = []
            |> f c.title
            |> f (g " " [c.given, c.second, c.family])
            |> f (g " " [c.street, c.street_number])
            |> f (g " " [g "-" [c.country_code, c.postal_code], c.city])
            |> f c.country

        main = Card.text [] main_

        company_ = []
            |> f c.company
            |> f c.company_address

        company =
            Card.text [] company_

        contact_fields = [c.phone, c.phone2, c.mobile, c.fax, c.fax2, c.mail
                            , c.mail2, c.web]

        contact_labels = ["Telefon", "Telefon", "Mobil", "Fax", "Fax", ""
                            , "", ""]

        h label value = case (label, value) of
            (l, "") -> []
            ("", v) -> [text v, br [] []]
            (l, v) -> List.map text [v, " (", l, ")"] ++ [br [] []]

        contact = Card.text []
            (List.concat (List.map2 h contact_labels contact_fields))

        contents =
            [ Card.title [ defaultCardTitle ] [ text c.keyword ]
            , main ]
            |> appendIfNotEmpty (c.company ++ c.company_address) [company]
            |> appendIfNotEmpty (String.join "" contact_fields)  [contact]
            |> append [ Card.actions [ defaultActions ] actions ]

    in
        Card.view [ defaultCard ] contents


view : Cfg msg -> Material.Model -> Model -> Html msg
view cfg mdl model =
    Html.map cfg.lift <| case model.editing of
        True  -> viewEdit cfg mdl model
        False -> viewShow cfg mdl model

