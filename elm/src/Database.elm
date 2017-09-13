module Database exposing
    ( getCustomerById
    , getPrevCustomerById
    , getNextCustomerById
    , getLatestCustomer
    , saveCustomer
    )

import Http
import Json.Encode as Encode
import Json.Decode as Decode

import Config
import Customer as C exposing (Customer)
import Booking  as B exposing (Booking)

res =
    { customers    = Config.apiUrl ++ "/customers"
    , bookings     = Config.apiUrl ++ "/bookings"
    , bIndividuals = Config.apiUrl ++ "/booked_individuals"
    , bRooms       = Config.apiUrl ++ "/booked_rooms"
    }

customerSelect : String
customerSelect =
    "select=*,bookings{*,booked_individuals{*},booked_rooms{*}}"

type alias Callback msg a = Result Http.Error a -> msg


---------------
-- Customers --
---------------

getCustomerById : Callback msg Customer -> Int -> Cmd msg
getCustomerById encap id =
    let params =
            [ "customer_id=eq." ++ (toString id)
            , customerSelect
            ]

        uri = buildUri res.customers params Nothing
    in
        Http.send encap (Http.get uri C.jsonDecoderFirst)

getPrevCustomerById : Callback msg Customer -> String -> Int -> Cmd msg
getPrevCustomerById encap filter id =
    let params =
            [ "customer_id=lt." ++ (toString id)
            , customerSelect
            , "order=customer_id.desc"
            , "limit=1"
            ]

        uri = buildUri res.customers params (Just filter)
    in
        Http.send encap (Http.get uri C.jsonDecoderFirst)

getNextCustomerById : Callback msg Customer -> String -> Int -> Cmd msg
getNextCustomerById encap filter id =
    let params =
            [ "customer_id=gt." ++ (toString id)
            , customerSelect
            , "order=customer_id.asc"
            , "limit=1"
            ]

        uri = buildUri res.customers params (Just filter)
    in
        Http.send encap (Http.get uri C.jsonDecoderFirst)

getLatestCustomer : Callback msg Customer -> String -> Cmd msg
getLatestCustomer encap filter =
    let params =
            [ "order=customer_id.desc"
            , customerSelect
            , "limit=1"
            ]

        uri = buildUri res.customers params (Just filter)
    in
        Http.send encap (Http.get uri C.jsonDecoderFirst)

patchCustomer : Callback msg Customer -> Customer -> Int -> Cmd msg
patchCustomer encap c id =
    let params =
            [ "customer_id=eq." ++ (toString id)
            ]

        uri = buildUri res.customers params Nothing

        cc = { c | customer_id = Just id }

        json = C.jsonEncode cc
    in
        Http.send encap (patchJson uri json C.jsonDecoderFirst)

saveCustomer : Callback msg Customer -> Customer -> Cmd msg
saveCustomer cb c =
    case c.customer_id of
        Nothing -> newCustomer cb c
        Just id -> patchCustomer cb c id

newCustomer : Callback msg Customer -> Customer -> Cmd msg
newCustomer encap c =
    let cc = { c | customer_id = Nothing }

        json = C.jsonEncode cc
    in
        Http.send encap (postJson res.customers json C.jsonDecoder)

patchBooking : Callback msg Booking -> Booking -> Int -> Cmd msg
patchBooking map b id =
    let json = { b | booking_id = Nothing } |> B.encode
        params = [ "booking_id=eq." ++ (toString id) ]
        uri = buildUri res.bookings params Nothing
    in
        Http.send map (patchJson uri json B.decode)

newBooking : Callback msg Booking -> Booking -> Cmd msg
newBooking map b =
    let json = { b | booking_id = Nothing } |> B.encode
    in
        Http.send map (postJson res.bookings json B.decode)



----------------------
-- Common Functions --
----------------------


buildUri : String -> List String -> Maybe String -> String
buildUri base params filter =
    let params_ =
            case String.trim (Maybe.withDefault "" filter) of
                "" -> params
                str -> let kw = Http.encodeUri "%" ++ str ++ "%" in
                    ("keyword=ilike." ++ kw) :: params
    in
        case String.join "&" params_ of
            "" -> base
            str -> String.concat [base, "?", str]


-- Helpers
----------

emptyJson : Encode.Value
emptyJson =
    Encode.object []

emptyJsonList : Encode.Value
emptyJsonList =
    Encode.list []


-- Http Requests
----------------

patchJson : String -> Encode.Value -> Decode.Decoder a -> Http.Request a
patchJson uri json decoder =
    let postgrestFullResponseHeader =
            Http.header "Prefer" "return=representation"
    in
        Http.request
            { method = "PATCH"
            , headers = [postgrestFullResponseHeader]
            , url = uri
            , body = Http.jsonBody json
            , expect = Http.expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }

postJson : String -> Encode.Value -> Decode.Decoder a -> Http.Request a
postJson uri json decoder =
    let postgrestFullResponseHeader =
            Http.header "Prefer" "return=representation"
    in
        Http.request
            { method = "POST"
            , headers = [postgrestFullResponseHeader]
            , url = uri
            , body = Http.jsonBody json
            , expect = Http.expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }
