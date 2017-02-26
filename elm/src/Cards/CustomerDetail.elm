module Cards.CustomerDetail exposing
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
import Material.Tabs as Tabs
import Material.Textfield as Textfield
import Material.Options as Options

import Defaults exposing (..)

import Customer exposing (Customer)

import Html exposing (Html, text, br)
import Html.Attributes as Attributes

type alias Model =
    { editMode : Bool
    , editTab : Int
    , cache : Customer
    }

type alias Cfg msg =
    { mdl        : Material.Model
    , mdlMessage : (Material.Msg msg -> msg)
    , index      : List Int
    , msg        : Msg -> msg
    , edit       : msg
    , done       : msg
    , delete     : msg
    }

type Msg
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

    | Phone           String
    | Phone2          String
    | Mobile          String
    | Fax             String
    | Fax2            String
    | Mail            String
    | Mail2           String
    | Web             String

    | Keyword         String

    | Abort
    | SelectTab       Int

show : Model
show =
    { editMode = False
    , editTab = 0
    , cache = Customer.empty
    }

edit : Customer -> Model
edit customer =
    { editMode = True
    , editTab = 0
    , cache = customer
    }

-- Update given Customer with editable fields from
-- cache TODO: if form is ok.
extract : Model -> Customer -> Maybe Customer
extract model customer =
    let c = model.cache
    in
        case model.editMode of
            False -> Nothing
            True -> Just
                { customer
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
                , note            = c.note
                }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let c = model.cache
        f c =
            ( { model | cache = c } , Cmd.none )
    in
        case msg of
            Title           s -> { c | title           = s } |> f
            Title_letter    s -> { c | title_letter    = s } |> f

            Given           s -> { c | given           = s } |> f
            Second          s -> { c | second          = s } |> f
            Family          s -> { c | family          = s } |> f

            Company         s -> { c | company         = s } |> f
            Company_address s -> { c | company_address = s } |> f

            Street          s -> { c | street          = s } |> f
            Street_number   s -> { c | street_number   = s } |> f
            City            s -> { c | city            = s } |> f
            Postal_code     s -> { c | postal_code     = s } |> f
            Country         s -> { c | country         = s } |> f
            Country_code    s -> { c | country_code    = s } |> f

            Phone           s -> { c | phone           = s } |> f
            Phone2          s -> { c | phone2          = s } |> f
            Mobile          s -> { c | mobile          = s } |> f
            Fax             s -> { c | fax             = s } |> f
            Fax2            s -> { c | fax2            = s } |> f
            Mail            s -> { c | mail            = s } |> f
            Mail2           s -> { c | mail2           = s } |> f
            Web             s -> { c | web             = s } |> f

            Keyword         s -> { c | keyword         = s } |> f

            Abort ->
                ( show, Cmd.none )

            SelectTab i ->
                let model_ =
                        { model | editTab = i }
                in
                    ( model_, Cmd.none )


viewEdit : Cfg msg -> Model -> Html msg
viewEdit cfg model =
    let index x = (x :: cfg.index)

        defaultButton_ i = defaultButton cfg.mdlMessage cfg.mdl (index i)

        actions = [ defaultButton_ 1 "done" cfg.done
                  , defaultButton_ 2 "cancel" (cfg.msg Abort)
                  , defaultButton_ 3 "delete" cfg.delete
                  ]

        tab_labels =
            [ "Name", "Adresse", "Kontakt" ]
            |> List.map (\l ->
                    Tabs.textLabel [ Options.center ] l )

        tf i label value msg =
            Textfield.render cfg.mdlMessage (index i) cfg.mdl
                [ Textfield.value (value model.cache)
                , Textfield.floatingLabel
                , Textfield.label (label)
                , Textfield.text_
                , Options.onInput (\t -> cfg.msg (msg t))
                ] ()

        -- TODO: Build nice gridded forms
        nameTab =
            [ tf 5  "Kürzel"       .keyword      Keyword
            , tf 6  "Anrede"       .title        Title
            , tf 7  "Anrede Brief" .title_letter Title_letter
            , tf 8  "Vorname"      .given        Given
            , tf 9  "Zweitname"    .second       Second
            , tf 10 "Nachname"     .family       Family
            ]

        addressTab =
            [ tf 21 "Straße"       .street        Street
            , tf 22 "Hausnummer"   .street_number Street_number
            , tf 23 "Ort"          .city          City
            , tf 24 "Postleitzahl" .postal_code   Postal_code
            , tf 25 "Land"         .country       Country
            , tf 26 "Ländercode"   .country_code  Country_code
            ]

        contactTab =
            [ tf 41 "Telefon"      .phone  Phone
            , tf 42 "Telefon"      .phone2 Phone2
            , tf 43 "Mobiltelefon" .mobile Mobile
            , tf 44 "Fax"          .fax    Fax
            , tf 45 "Fax"          .fax2   Fax2
            , tf 46 "Email"        .mail   Mail
            , tf 47 "Email"        .mail2  Mail2
            , tf 48 "Website"      .web    Web
            ]

        bundle = Html.div []

        tabs = Tabs.render cfg.mdlMessage (index 4) cfg.mdl
                [ Tabs.onSelectTab (\i -> cfg.msg (SelectTab i))
                , Tabs.activeTab model.editTab
                ]
                tab_labels
                [ case model.editTab of
                    1 -> bundle addressTab
                    2 -> bundle contactTab
                    _ -> bundle nameTab
                ]

        cardContent =
            [ Card.title [ defaultCardTitle ] [ text model.cache.keyword ]
            , Card.text [] [ tabs ]
            , Card.actions [ defaultActions ] actions
            ]
    in
        Card.view [ defaultCard ] cardContent


viewShow : Cfg msg -> Customer -> Html msg
viewShow cfg customer =
    let i x = (x :: cfg.index)

        defaultButton_ x = defaultButton cfg.mdlMessage cfg.mdl (i x)

        actions =
            [ defaultButton_ 1 "mode_edit" cfg.edit
            , defaultButton_ 2 "delete"    cfg.delete
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

        c = customer

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


view : Cfg msg -> Model -> Customer -> Html msg
view cfg model customer =
    case model.editMode of
        True  -> viewEdit cfg model
        False -> viewShow cfg customer

