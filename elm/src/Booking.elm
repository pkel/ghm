module Booking exposing
    ( Booking
    , BookedIndividual
    , BookedRoom
    , decode
    , empty
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Json.Encode.Extra as EncodeX

import JsonHelpers
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
    , factor        : Float
    , description   : String
    , breakfast     : Bool
    , note          : String
    , from_date     : Maybe Date
    , to_date       : Maybe Date
    }

type alias Booking =
    { booking_id       : Maybe Int
    , state            : Int
    , deposit_asked    : Maybe Float
    , deposit_git      : Maybe Float
    , no_tax           : Bool
    , note             : String
    , individuals      : List BookedIndividual
    , rooms            : List BookedRoom
    }


-- Json

decode : Decoder Booking
decode =
    let required  = Pipeline.required
        optional  = Pipeline.optional
        hardcoded = Pipeline.hardcoded
        nullable  = Decode.nullable
        int       = Decode.int
        float     = Decode.float
        bool      = Decode.bool
        string    = Decode.string
        list      = Decode.list
    in
        Pipeline.decode Booking
            |> required "booking_id"         (nullable int)
            |> optional "state"              int 0
            |> optional "deposit_asked"      (nullable float) Nothing
            |> optional "deposit_got"        (nullable float) Nothing
            |> optional "no_tax"             bool False
            |> optional "note"               string ""
            |> optional "booked_individuals" (list decodeIndividual) []
            |> optional "booked_rooms"       (list decodeRoom) []

decodeIndividual : Decoder BookedIndividual
decodeIndividual =
    let optional = Pipeline.optional
        nullable = Decode.nullable
        int      = Decode.int
        string   = Decode.string
    in
        Pipeline.decode BookedIndividual
            |> optional "given"         string ""
            |> optional "second"        string ""
            |> optional "family"        string ""
            |> optional "year_of_birth" (nullable int) Nothing
            |> optional "month_of_bith" (nullable int) Nothing
            |> optional "day_of_bith"   (nullable int) Nothing

decodeRoom : Decoder BookedRoom
decodeRoom =
    let optional = Pipeline.optional
        nullable = Decode.nullable
        float    = Decode.float
        bool     = Decode.bool
        int      = Decode.int
        date     = JsonHelpers.decodeDate
        string   = Decode.string
    in
        -- TODO: Read defaults from config / database
        Pipeline.decode BookedRoom
            |> optional "room"          (nullable int) Nothing
            |> optional "beds"          int 2
            |> optional "price_per_bed" float 0.0
            |> optional "factor"        float 1.0
            |> optional "description"   string ""
            |> optional "breakfast"     bool True
            |> optional "note"          string ""
            |> optional "from_date"     (nullable date) Nothing
            |> optional "to_date"       (nullable date) Nothing


-- Constructors

emptyIndividual : () -> BookedIndividual
emptyIndividual () =
    BookedIndividual "" "" "" Nothing Nothing Nothing

-- TODO: Read default from config / database
emptyRoom : () -> BookedRoom
emptyRoom () =
    BookedRoom Nothing 2 0.0 1.0 "" True "" Nothing Nothing

empty : () -> Booking
empty () =
    Booking Nothing 0 Nothing Nothing False "" [] []

