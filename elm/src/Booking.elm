module Booking exposing
    ( Booking
    , BookedIndividual
    , BookedRoom
    , jsonDecoder
    , jsonDecoderFirst
    , empty
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Json.Encode.Extra as EncodeX

import Date exposing (Date)

type alias BookedIndividual =
    { given         : String
    , second        : String
    , family        : String
    , year_of_birth : Maybe Int
    , month_of_bith : Maybe Int
    , day_of_bith   : Maybe Int
    }

type alias BookedRoom =
    { room          : Maybe Int
    , beds          : Int
    , price_per_bed : Float
    , factor        : Int
    , description   : String
    , breakfast     : Bool
    , note          : String
    , from_date     : Date
    , to_date       : Date
    }

type alias Booking =
    { booking_id       : Maybe Int
    , state            : Int
    , deposit_asked    : Float
    , deposit_git      : Float
    , no_tax           : Bool
    , note             : String
    , individuals      : List BookedIndividual
    , rooms            : List BookedRoom
    }


-- Json

jsonDecoderFirst : Decoder Booking
jsonDecoderFirst =
  Decode.index 0 jsonDecoder

jsonDecoder : Decoder Booking
jsonDecoder =
    let required = Pipeline.required
        optional = Pipeline.optional
        hardcoded = Pipeline.hardcoded
        nullable = Decode.nullable
        int      = Decode.int
        float    = Decode.float
        bool     = Decode.bool
        string   = Decode.string
    in
        Pipeline.decode Booking
            |> required "booking_id"       (nullable int)
            |> optional "state"            int       0
            |> optional "deposit_asked"    float     0.0
            |> optional "deposit_got"      float     0.0
            |> optional "no_tax"           bool      False
            |> optional "note"             string    ""
            -- TODO: Individuals and rooms are not decoded yet:
            |> hardcoded []
            |> hardcoded []


-- Constructors

emptyIndividual : () -> BookedIndividual
emptyIndividual () =
    BookedIndividual "" "" "" Nothing Nothing Nothing

empty : () -> Booking
empty () =
    Booking Nothing 0 0.0 0.0 False "" [] []

