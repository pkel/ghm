module Booking exposing
    ( Booking
    , Individual
    , Room
    , Summary
    , summary
    , decode
    , empty
    , emptyIndividual
    , emptyRoom
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Json.Encode.Extra as EncodeX

import Helpers.List as ListH
import Helpers.Json as JsonH

import Date exposing (Date)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- Types

type alias Individual =
    { given          : String
    , second         : String
    , family         : String
    , date_of_birth  : Maybe Date
    }

type alias Room =
    { room          : Maybe Int
    , beds          : Int
    , price_per_bed : Float
    , factor        : Float
    , description   : String
    , breakfast     : Bool
    , note          : String
    , from          : Maybe Date
    , to            : Maybe Date
    }

type alias Booking =
    { booking_id       : Maybe Int
    , state            : Int
    , deposit_asked    : Maybe Float
    , deposit_git      : Maybe Float
    , no_tax           : Bool
    , note             : String
    , individuals      : List Individual
    , rooms            : List Room
    }

type alias Summary =
    { from      : Maybe Date
    , to        : Maybe Date
    , n_rooms   : Int
    , n_beds    : Int
    }


-- Functions

-- Maximal Time Frame and sum of beds/rooms
summary : Booking -> Summary
summary b =
    let f a b   = Maybe.map Date.toTime (a b)
        from    = List.map (f .from) b.rooms |> ListH.minimumNotNothing
        to      = List.map (f .to)   b.rooms |> ListH.maximumNotNothing
        n_rooms = List.length b.rooms
        n_beds  = List.map .beds b.rooms |> List.foldl (+) 0
        conv    = Maybe.map Date.fromTime
    in
        Summary (conv from) (conv to) n_rooms n_beds


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

decodeIndividual : Decoder Individual
decodeIndividual =
    let optional = Pipeline.optional
        nullable = Decode.nullable
        date     = JsonH.decodeDate
        string   = Decode.string
    in
        Pipeline.decode Individual
            |> optional "given"         string ""
            |> optional "second"        string ""
            |> optional "family"        string ""
            |> optional "date_of_birth" (nullable date) Nothing

decodeRoom : Decoder Room
decodeRoom =
    let optional = Pipeline.optional
        nullable = Decode.nullable
        float    = Decode.float
        bool     = Decode.bool
        int      = Decode.int
        date     = JsonH.decodeDate
        string   = Decode.string
    in
        -- TODO: Read defaults from config / database
        Pipeline.decode Room
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

emptyIndividual :  Individual
emptyIndividual =
    Individual "" "" "" Nothing

-- TODO: Read default from config / database
emptyRoom : Room
emptyRoom =
    Room Nothing 2 0.0 1.0 "" True "" Nothing Nothing

empty : Booking
empty =
    Booking Nothing 0 Nothing Nothing False "" [] []

