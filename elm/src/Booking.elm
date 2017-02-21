module Booking exposing
    ( Booking
    , BookedIndividual
    , BookedRoom
    , Summary
    , summary
    , birthday
    , view
    , decode
    , empty
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

type alias BookedIndividual =
    { given          : String
    , second         : String
    , family         : String
    , year_of_birth  : Maybe Int
    , month_of_birth : Maybe Int
    , day_of_birth   : Maybe Int
    }

type alias BookedRoom =
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
    , individuals      : List BookedIndividual
    , rooms            : List BookedRoom
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

birthday : BookedIndividual -> String
birthday i =
    let f x = Maybe.withDefault ""  (Maybe.map toString x)
    in
    case (i.day_of_birth, i.month_of_birth, i.year_of_birth) of
        (Nothing, Nothing, Nothing) -> "n/a"
        (d, m, y) ->
            String.join "."
                ( List.map f [d, m, y] )


-- Html representation
-- TODO: Get rid of these

view : Booking -> Html msg
view booking =
    let individuals =
            div [class "persons"] (List.map viewIndividual booking.individuals)
        rooms =
            div [class "rooms"] (List.map viewRoom booking.rooms)
    in
    div [class "booking" ] [individuals, rooms]

viewIndividual : BookedIndividual -> Html msg
viewIndividual individual =
    let i = individual
        g = Maybe.withDefault ""
        h = Maybe.map toString
        j = Maybe.map (\s -> s ++ ".")
        birthday =
            String.join "" ( List.map g
                [ j (h i.day_of_birth)
                , j (h i.month_of_birth)
                , h i.year_of_birth
                ] )
        name =
            String.join " "
                [ i.given
                , i.second
                , i.family
                ]
    in
        div [ class "person" ]
            [ span [class "name"] [text name]
            , span [class "birthday"] [text birthday]
            ]

viewRoom : BookedRoom -> Html msg
viewRoom room =
    let r = room
        num = Maybe.withDefault "" (Maybe.map toString r.room)
        price_ = toString r.price_per_bed ++ "€"
        factor = toString (round (r.factor * 100)) ++ "%"
        price = price_ ++ " (" ++ factor ++ ")"
    in
        div [ class "room" ]
            [ text ("Nummer:" ++ num) -- TODO: Fetch room details
            , br [] []
            , text r.description
            , br [] []
            , text price
            , p [] [text r.note]
            ]

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
        date     = JsonH.decodeDate
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

