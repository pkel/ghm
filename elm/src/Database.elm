module Database exposing
    ( getCustomerById
    , getPrevCustomerById
    , getNextCustomerById
    , getLatestCustomer
    , saveCustomer
    , newCustomer
    )

import Http
import Json.Encode as Encode
import Json.Decode as Decode

import Config
import Customer as C exposing (Customer)

customersUri : String
customersUri =
    Config.apiUrl ++ "/customers"

customerSelect : String
customerSelect =
    "select=*,bookings{*,booked_individuals{*},booked_rooms{*}}"


---------------
-- Customers --
---------------

getCustomerById : (Result Http.Error Customer -> msg) -> Int -> Cmd msg
getCustomerById encap id =
    let params =
            [ "customer_id=eq." ++ (toString id)
            , customerSelect
            ]

        uri = buildUri customersUri params Nothing
    in
        Http.send encap (Http.get uri C.jsonDecoderFirst)

getPrevCustomerById : (Result Http.Error Customer -> msg) -> String -> Int -> Cmd msg
getPrevCustomerById encap filter id =
    let params =
            [ "customer_id=lt." ++ (toString id)
            , customerSelect
            , "order=customer_id.desc"
            , "limit=1"
            ]

        uri = buildUri customersUri params (Just filter)
    in
        Http.send encap (Http.get uri C.jsonDecoderFirst)

getNextCustomerById : (Result Http.Error Customer -> msg) -> String -> Int -> Cmd msg
getNextCustomerById encap filter id =
    let params =
            [ "customer_id=gt." ++ (toString id)
            , customerSelect
            , "order=customer_id.asc"
            , "limit=1"
            ]

        uri = buildUri customersUri params (Just filter)
    in
        Http.send encap (Http.get uri C.jsonDecoderFirst)

getLatestCustomer : (Result Http.Error Customer -> msg) -> String -> Cmd msg
getLatestCustomer encap filter =
    let params =
            [ "order=customer_id.desc"
            , customerSelect
            , "limit=1"
            ]

        uri = buildUri customersUri params (Just filter)
    in
        Http.send encap (Http.get uri C.jsonDecoderFirst)

saveCustomer : (Result Http.Error Customer -> msg) -> Customer -> Int -> Cmd msg
saveCustomer encap c id =
    let params =
            [ "customer_id=eq." ++ (toString id)
            ]

        uri = buildUri customersUri params Nothing

        cc = { c | customer_id = Just id }

        json = C.jsonEncode cc
    in
        Http.send encap (patchJson uri json C.jsonDecoderFirst)

newCustomer : (Result Http.Error Customer -> msg) -> Customer -> Cmd msg
newCustomer encap c =
    let cc = { c | customer_id = Nothing }

        json = C.jsonEncode cc
    in
        Http.send encap (postJson customersUri json C.jsonDecoder)


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
