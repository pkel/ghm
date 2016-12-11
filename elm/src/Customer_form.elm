module Customer_form exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Customer_t exposing (Customer)
import Customer_t as CType

-- MODEL

type alias Model = Customer

-- UPDATE

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
  | Note            String

update: Msg -> Model -> Model
update msg model =
  case msg of
    Title           str -> { model | title           = str }
    Title_letter    str -> { model | title_letter    = str }

    Given           str -> { model | given           = str }
    Second          str -> { model | second          = str }
    Family          str -> { model | family          = str }

    Company         str -> { model | company         = str }
    Company_address str -> { model | company_address = str }

    Street          str -> { model | street          = str }
    Street_number   str -> { model | street_number   = str }
    City            str -> { model | city            = str }
    Postal_code     str -> { model | postal_code     = str }
    Country         str -> { model | country         = str }
    Country_code    str -> { model | country_code    = str }

    Phone           str -> { model | phone           = str }
    Phone2          str -> { model | phone2          = str }
    Mobile          str -> { model | mobile          = str }
    Fax             str -> { model | fax             = str }
    Fax2            str -> { model | fax2            = str }
    Mail            str -> { model | mail            = str }
    Mail2           str -> { model | mail2           = str }
    Web             str -> { model | web             = str }

    Keyword         str -> { model | keyword         = str }
    Note            str -> { model | note            = str }



-- Validation
-- Conceptually, this makes sense for forms. But not on this one (yet)

validation: Model -> Bool
validation m =
  True

-- Extraction
-- Even though, we do not own the state, we want to make sure,
-- that others only use validated data

extract: Model -> Maybe Customer
extract m =
  if validation m then Just m else Nothing

-- VIEW

-- TODO: This function should be in a Module called html elements
input_field title action val =
  label []
  [ text title
  , br [] []
  , input [ type_ "text", onInput action, value val ] []
  , br [] []
  ]

titled_fieldset title elements =
  fieldset [] (legend [] [text title] :: elements)

view : (Msg -> msg) -> Model -> Html msg
view encapsulate model =
  Html.map encapsulate (viewForm model)

viewForm : Model -> Html Msg
viewForm model =
  Html.form []
  [   titled_fieldset "Anrede"
    [ input_field     "Anrede"       Title           model.title
    , input_field     "Anrede Brief" Title_letter    model.title_letter
    ]
  ,   titled_fieldset "Name"
    [ input_field     "Vorname"      Given           model.given
    , input_field     "Zweitname"    Second          model.second
    , input_field     "Nachnahme"    Family          model.family
    , viewValidation  model
    ]
  ,   titled_fieldset "Firma"
    [ input_field     "Firma"        Company         model.company
    , input_field     "Adresse"      Company_address model.company_address
    ]
  ,   titled_fieldset "Suche"
    [ input_field     "Kürzel"       Keyword         model.keyword
    ]
  ,   titled_fieldset "Adresse"
    [ input_field     "Straße"       Street          model.street
    , input_field     "Hausnummer"   Street_number   model.street_number
    , input_field     "Ort"          City            model.city
    , input_field     "Postleitzahl" Postal_code     model.postal_code
    , input_field     "Land"         Country         model.country
    , input_field     "Ländercode"   Country_code    model.country_code
    ]
  ,   titled_fieldset "Kontakt"
    [ input_field     "Telefon"      Phone           model.phone
    , input_field     "Telefon2"     Phone2          model.phone2
    , input_field     "Mobiltelefon" Mobile          model.mobile
    , input_field     "Fax"          Fax             model.fax
    , input_field     "Fax2"         Fax2            model.fax2
    , input_field     "Mail"         Mail            model.mail
    , input_field     "Mail2"        Mail2           model.mail2
    , input_field     "Internet"     Web             model.web
    ]
  ,   titled_fieldset "Sonstiges"
    [ textarea [ onInput Note, value model.note ] []
    ]
  ]

-- This validation is nonsense ...
viewValidation : Model -> Html Msg
viewValidation model =
  let (color, message) =
    if String.length (String.trim model.family) == 0
       then ("red", "Mindestens ein Nachnahme sollte angegeben werden")
       else ("green", "Eingabe OK")
  in
  div [ style [("color", color)] ] [ text message ]

