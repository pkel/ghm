module Customer_form exposing (..)

import Html exposing (Html)
import Pure exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Customer_t exposing (Customer)
import Customer_t as CType

-- MODEL

type alias Model = Customer

init : Customer -> Model
init customer =
  customer

initEmpty : () -> Model
initEmpty =
  CType.empty

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

titled_fieldset title elements =
  fieldset [] (legend [] [text title] :: elements)

view : (Msg -> msg) -> Model -> Html msg
view encapsulate model =
  Html.map encapsulate (viewForm model)

viewForm : Model -> Html Msg
viewForm model =
  let tf = Pure.text_field in
  let title_fields =
    [ tf     "Anrede"       Title           model.title
    , tf     "Anrede Brief" Title_letter    model.title_letter
    ]
  in
  let name_fields =
    [ tf     "Vorname"      Given           model.given
    -- , tf     "Zweitname"    Second          model.second
    , tf     "Nachname"    Family          model.family
    ]
  in
  let company_fields =
    [ tf     "Firma"        Company         model.company
    , tf     "Adresse"      Company_address model.company_address
    ]
  in
  let search_fields =
    [ tf     "Kürzel"       Keyword         model.keyword
    ]
  in
  let address_fiels =
    [ tf     "Straße"       Street          model.street
    , tf     "Hausnummer"   Street_number   model.street_number
    , tf     "Ort"          City            model.city
    , tf     "Postleitzahl" Postal_code     model.postal_code
    , tf     "Land"         Country         model.country
    , tf     "Ländercode"   Country_code    model.country_code
    ]
  in
  let contact_fields =
    [ tf     "Telefon"      Phone           model.phone
    , tf     "Telefon"      Phone2          model.phone2
    , tf     "Mobiltelefon" Mobile          model.mobile
    , tf     "Fax"          Fax             model.fax
    , tf     "Fax"          Fax2            model.fax2
    , tf     "Mail"         Mail            model.mail
    , tf     "Mail"         Mail2           model.mail2
    , tf     "Internet"     Web             model.web
    ]
  in
  let additional_fields =
    [ textarea [ onInput Note, value model.note, class "pure-input-1" ] []
    ]
  in
  let left =
    [ titled_fieldset "Anrede" title_fields
    , titled_fieldset "Name" name_fields
    , titled_fieldset "Firma" company_fields
    ]
  in
  let middle =
    [ titled_fieldset "Suche" search_fields
    , titled_fieldset "Adresse" address_fiels
    ]
  in
  let right =
    [ titled_fieldset "Kontakt" contact_fields
    ]
  in
  let bottom =
    titled_fieldset "Sonstiges" additional_fields
  in
  let upper =
    Pure.group (List.map (Pure.element 1 3) [left,middle,right])
  in
  Pure.form [] [upper, bottom]
