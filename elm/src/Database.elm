module Database exposing
    ( getCustomerById
    , getPrevCustomerById
    , getNextCustomerById
    , getLatestCustomer
    , saveCustomer
    , Msg(..)
    , State(..)
    , init
    , Model
    , DbError
    , update
    , state
    )

import Http
import Json.Encode as Encode
import Json.Decode as Decode
import Material.Helpers exposing (pure, effect)

import Config
import Customer as C exposing (Customer)
import Booking  as B exposing (Booking)

res =
    { customers = Config.apiUrl ++ "/customers"
    , bookings  = Config.apiUrl ++ "/bookings"
    }

customerSelect : String
customerSelect =
    "select=*,bookings{*}"

type Msg
  = DbReturnCustomer Customer
  | DbSavedCustomer
  | DbMsg InternalMsg

type alias DbError =
  { action : String
  , http_err : Http.Error
  }

type InternalMsg
    = CustomerCreated Int
    | RoomsOK
    | Error DbError
    | Success

type State
  = Idle
  | Busy
  | Failed (List DbError)

type alias Model =
  { errors : List DbError
  , task_count : Int
  }

init : Model
init =
  { errors = []
  , task_count = 0
  }

state : Model -> State
state m =
  case m.errors of
    [] -> if m.task_count > 0 then Busy else Idle
    l  -> Failed l

update : Model -> InternalMsg -> (Model, Cmd Msg)
update m imsg =
  case imsg of
    --TODO: Obviously not finished
    CustomerCreated i -> pure m
    RoomsOK -> pure m
    Success -> pure m
    Error e -> { m | errors = e :: m.errors } |> pure

error : String -> Http.Error -> Msg
error action err =
  { action = action
  , http_err = err
  } |> Error |> DbMsg


---------------
-- Customers --
---------------

getCustomerById : Int -> Cmd Msg
getCustomerById id =
    let params =
            [ "customer_id=eq." ++ (toString id)
            , customerSelect
            ]

        uri = buildUri res.customers params Nothing
        return res =
          case res of
            Err e -> error "getCustomerById" e
            Ok  c -> DbReturnCustomer c
    in
        Http.get uri C.jsonDecoderFirst
        |> Http.send return

getPrevCustomerById : String -> Int -> Cmd Msg
getPrevCustomerById filter id =
    let params =
            [ "customer_id=lt." ++ (toString id)
            , customerSelect
            , "order=customer_id.desc"
            , "limit=1"
            ]

        uri = buildUri res.customers params (Just filter)
        return res =
          case res of
            Err e -> error "getPrevCustomerById" e
            Ok  c -> DbReturnCustomer c
    in
        Http.get uri C.jsonDecoderFirst
        |> Http.send return

getNextCustomerById : String -> Int -> Cmd Msg
getNextCustomerById filter id =
    let params =
            [ "customer_id=gt." ++ (toString id)
            , customerSelect
            , "order=customer_id.asc"
            , "limit=1"
            ]

        uri = buildUri res.customers params (Just filter)
        return res =
          case res of
            Err e -> error "getNextCustomerById" e
            Ok  c -> DbReturnCustomer c
    in
        Http.get uri C.jsonDecoderFirst
        |> Http.send return

getLatestCustomer : String -> Cmd Msg
getLatestCustomer filter =
    let params =
            [ "order=customer_id.desc"
            , customerSelect
            , "limit=1"
            ]

        uri = buildUri res.customers params (Just filter)
        return res =
          case res of
            Err e -> error "getLatestCustomer" e
            Ok  c -> DbReturnCustomer c
    in
        Http.get uri C.jsonDecoderFirst
        |> Http.send return


saveCustomer : Customer -> Cmd Msg
saveCustomer c =
  case c.customer_id of
    Nothing -> createCustomer   c
    Just id -> patchCustomer id c

patchCustomer : Int -> Customer -> Cmd Msg
patchCustomer id c =
    let params =
            [ "customer_id=eq." ++ (toString id)
            ]

        uri = buildUri res.customers params Nothing

        json = C.jsonEncode c
        return res =
          case res of
            Err e -> error "patchCustomer" e
            -- TODO: Save Bookings
            Ok  _ -> DbSavedCustomer
    in
        patchJson uri json C.jsonDecoderFirst
        |> Http.send return

createCustomer : Customer -> Cmd Msg
createCustomer c =
    let json = C.jsonEncode c
        return res =
          case res of
            Err e -> error "createCustomer" e
            -- TODO: Save Bookings
            Ok  _ -> DbSavedCustomer
    in
        postJson res.customers json C.jsonDecoder
        |> Http.send return

saveBooking : Int -> Booking -> Cmd Msg
saveBooking customer_id b =
  let b_ = { b | customer_id = Just customer_id }
  in case b.booking_id of
      Nothing -> createBooking b_
      Just id -> patchBooking b_ id

patchBooking : Booking -> Int -> Cmd Msg
patchBooking b id =
    let params = [ "booking_id=eq." ++ (toString id) ]
        uri = buildUri res.bookings params Nothing
        json = B.encode b
        return res =
          case res of
            Err e -> error "patchBooking" e
            Ok  _ -> DbMsg Success
    in
        patchJson uri json B.decode
        |> Http.send return

createBooking : Booking -> Cmd Msg
createBooking b =
    let json = B.encode b
        return res =
          case res of
            Err e -> error "createBooking" e
            Ok  _ -> DbMsg Success
    in
        postJson res.bookings json B.decode
        |> Http.send return



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
