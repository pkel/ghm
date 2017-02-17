module BookingForm exposing (..)

import Html exposing (Html)
import Pure exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Booking as B exposing (Booking)

-- MODEL

type alias Model = Booking

init : Booking -> Model
init booking =
        booking

initEmpty : () -> Model
initEmpty =
        B.empty

-- UPDATE

type Msg
    = Note            String
    -- | Title_letter    String

    -- | Given           String
    -- | Second          String
    -- | Family          String

    -- | Company         String
    -- | Company_address String

    -- | Street          String
    -- | Street_number   String
    -- | City            String
    -- | Postal_code     String
    -- | Country         String
    -- | Country_code    String

    -- | Phone           String
    -- | Phone2          String
    -- | Mobile          String
    -- | Fax             String
    -- | Fax2            String
    -- | Mail            String
    -- | Mail2           String
    -- | Web             String

    -- | Keyword         String
    -- | Note            String

update: Msg -> Model -> Model
update msg model =
    case msg of
    --     Title           str -> { model | title           = str }
    --     Title_letter    str -> { model | title_letter    = str }

    --     Given           str -> { model | given           = str }
    --     Second          str -> { model | second          = str }
    --     Family          str -> { model | family          = str }

    --     Company         str -> { model | company         = str }
    --     Company_address str -> { model | company_address = str }

    --     Street          str -> { model | street          = str }
    --     Street_number   str -> { model | street_number   = str }
    --     City            str -> { model | city            = str }
    --     Postal_code     str -> { model | postal_code     = str }
    --     Country         str -> { model | country         = str }
    --     Country_code    str -> { model | country_code    = str }

    --     Phone           str -> { model | phone           = str }
    --     Phone2          str -> { model | phone2          = str }
    --     Mobile          str -> { model | mobile          = str }
    --     Fax             str -> { model | fax             = str }
    --     Fax2            str -> { model | fax2            = str }
    --     Mail            str -> { model | mail            = str }
    --     Mail2           str -> { model | mail2           = str }
    --     Web             str -> { model | web             = str }

    --     Keyword         str -> { model | keyword         = str }
        Note            str -> { model | note            = str }



-- Validation
-- Conceptually, this makes sense for forms. But not on this one (yet)

validation: Model -> Bool
validation m =
    True

-- Extraction
-- Even though, we do not own the state, we want to make sure,
-- that others only use validated data

extract: Model -> Maybe Booking
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
    let tf = Pure.titledTextfield
        s = B.summary model

        additional_fields =
            [ textarea [ onInput Note, value model.note, class "pure-input-1" ] []
            ]

        note =
            titled_fieldset "Sonstiges" additional_fields

    in
        Pure.form [] [note]
