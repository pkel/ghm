module Customer exposing
    ( Customer
    , jsonDecoder
    , jsonDecoderFirst
    , jsonEncode
    , empty
    , setBookings
    , setNote
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Json.Encode.Extra as EncodeX

import Booking as B exposing (Booking)

import Html exposing (..)

type alias Customer =
    { customer_id      : Maybe Int

    , title            : String
    , title_letter     : String

    , given            : String
    , second           : String
    , family           : String

    , company          : String
    , company_address  : String

    , street           : String
    , street_number    : String
    , postal_code      : String
    , city             : String
    , country          : String
    , country_code     : String

    , phone            : String
    , phone2           : String
    , mobile           : String
    , fax              : String
    , fax2             : String
    , mail             : String
    , mail2            : String
    , web              : String

    , keyword          : String
    , note             : String

    , bookings         : List Booking
    }


-- Setter

setBookings : Customer -> List Booking -> Customer
setBookings customer bookings =
    { customer | bookings = bookings }


setNote : Customer -> String -> Customer
setNote customer note =
    { customer | note = note }

-- Json

jsonDecoderFirst : Decoder Customer
jsonDecoderFirst =
    Decode.index 0 jsonDecoder

jsonDecoder : Decoder Customer
jsonDecoder =
    let required = Pipeline.required
        optional = Pipeline.optional
        nullable = Decode.nullable
        int      = Decode.int
        string   = Decode.string
        list     = Decode.list
        booking  = B.decode
    in
        Pipeline.decode Customer
            |> required "customer_id"      (nullable int)

            |> optional "title"            string    ""
            |> optional "title_letter"     string    ""

            |> optional "given"            string    ""
            |> optional "second"           string    ""
            |> optional "family"           string    ""

            |> optional "company"          string    ""
            |> optional "company_address"  string    ""

            |> optional "street"           string    ""
            |> optional "street_number"    string    ""
            |> optional "postal_code"      string    ""
            |> optional "city"             string    ""
            |> optional "country"          string    ""
            |> optional "country_code"     string    ""

            |> optional "phone"            string    ""
            |> optional "phone2"           string    ""
            |> optional "mobile"           string    ""
            |> optional "fax"              string    ""
            |> optional "fax2"             string    ""
            |> optional "mail"             string    ""
            |> optional "mail2"            string    ""
            |> optional "web"              string    ""

            |> optional "keyword"          string    ""
            |> optional "note"             string    ""

            |> optional "bookings"         (list booking) []


jsonEncode : Customer -> Encode.Value
jsonEncode c =
    let int    = Encode.int
        maybe  = EncodeX.maybe
        string = Encode.string
    in
        Encode.object
            [ ("customer_id",      (maybe int) c.customer_id)

            , ("title",            string c.title)
            , ("title_letter",     string c.title_letter)

            , ("given",            string c.given)
            , ("second",           string c.second)
            , ("family",           string c.family)

            , ("company",          string c.company)
            , ("company_address",  string c.company_address)

            , ("street",           string c.street)
            , ("street_number",    string c.street_number)
            , ("postal_code",      string c.postal_code)
            , ("city",             string c.city)
            , ("country",          string c.country)
            , ("country_code",     string c.country_code)

            , ("phone",            string c.phone)
            , ("phone2",           string c.phone2)
            , ("mobile",           string c.mobile)
            , ("fax",              string c.fax)
            , ("fax2",             string c.fax2)
            , ("mail",             string c.mail)
            , ("mail2",            string c.mail2)
            , ("web",              string c.web)

            , ("keyword",          string c.keyword)
            , ("note",             string c.note)
            ]


-- Constructors

empty : Customer
empty =
    { customer_id      = Nothing

    , title            = ""
    , title_letter     = ""

    , given            = ""
    , second           = ""
    , family           = ""

    , company          = ""
    , company_address  = ""

    , street           = ""
    , street_number    = ""
    , postal_code      = ""
    , city             = ""
    , country          = ""
    , country_code     = ""

    , phone            = ""
    , phone2           = ""
    , mobile           = ""
    , fax              = ""
    , fax2             = ""
    , mail             = ""
    , mail2            = ""
    , web              = ""

    , keyword          = ""
    , note             = ""

    , bookings         = []
    }

