module Database exposing
    ( getCustomerById
    , getPrevCustomerById
    , getNextCustomerById
    , getLatestCustomer
    , patchCustomer
    , createCustomer
    , saveBooking
    , Msg(..)
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
    }

customerSelect : String
customerSelect =
    "select=*,bookings{*}"

type Msg
    = DbError Http.Error
    | DbReceived Customer
    | DbCustomerCreated Int
    | DbSuccess

type alias Callback msg = Msg -> msg

map f ret =
    case ret of
        Err e -> DbError e
        Ok  x -> f x

customer : Callback msg -> Result Http.Error Customer -> msg
customer cb r =
    map DbReceived r |> cb

unit : Callback msg -> Result Http.Error a -> msg
unit cb r =
    map (\x -> DbSuccess) r |> cb

send cb wrap =
    Http.send (wrap cb)

---------------
-- Customers --
---------------

getCustomerById : Callback msg -> Int -> Cmd msg
getCustomerById cb id =
    let params =
            [ "customer_id=eq." ++ (toString id)
            , customerSelect
            ]

        uri = buildUri res.customers params Nothing
    in
        Http.send (customer cb) (Http.get uri C.jsonDecoderFirst)

getPrevCustomerById : Callback msg -> String -> Int -> Cmd msg
getPrevCustomerById cb filter id =
    let params =
            [ "customer_id=lt." ++ (toString id)
            , customerSelect
            , "order=customer_id.desc"
            , "limit=1"
            ]

        uri = buildUri res.customers params (Just filter)
    in
        Http.get uri C.jsonDecoderFirst
        |> send cb customer

getNextCustomerById : Callback msg -> String -> Int -> Cmd msg
getNextCustomerById cb filter id =
    let params =
            [ "customer_id=gt." ++ (toString id)
            , customerSelect
            , "order=customer_id.asc"
            , "limit=1"
            ]

        uri = buildUri res.customers params (Just filter)
    in
        Http.get uri C.jsonDecoderFirst
        |> send cb customer

getLatestCustomer : Callback msg -> String -> Cmd msg
getLatestCustomer cb filter =
    let params =
            [ "order=customer_id.desc"
            , customerSelect
            , "limit=1"
            ]

        uri = buildUri res.customers params (Just filter)
    in
        Http.get uri C.jsonDecoderFirst
        |> send cb customer

patchCustomer : Callback msg -> Customer -> Int -> Cmd msg
patchCustomer cb c id =
    let params =
            [ "customer_id=eq." ++ (toString id)
            ]

        uri = buildUri res.customers params Nothing

        json = C.jsonEncode c
    in
        patchJson uri json C.jsonDecoderFirst
        |> send cb unit

createCustomer : Callback msg -> Customer -> Cmd msg
createCustomer cb c =
    let json = C.jsonEncode c
    in
        -- TODO: Return CustomerCreated id
        postJson res.customers json C.jsonDecoder
        |> send cb unit

saveBooking : Callback msg -> Int -> Booking -> Cmd msg
saveBooking cb customer_id b =
  let b_ = { b | customer_id = Just customer_id }
  in case b.booking_id of
      Nothing -> createBooking cb b_
      Just id -> patchBooking cb b_ id

patchBooking : Callback msg -> Booking -> Int -> Cmd msg
patchBooking cb b id =
    let params = [ "booking_id=eq." ++ (toString id) ]
        uri = buildUri res.bookings params Nothing
        json = B.encode b
    in
        patchJson uri json B.decode
        |> send cb unit

createBooking : Callback msg -> Booking -> Cmd msg
createBooking cb b =
    let json = B.encode b
    in
        postJson res.bookings json B.decode
        |> send cb unit



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
