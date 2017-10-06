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
  | DbMsg InternalMsg

type alias DbError =
  { action : String
  , http_err : Maybe Http.Error
  }

type InternalMsg
    = Error DbError
    | SaveBookings Int (List Booking)

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
    Error e -> { m | errors = (Debug.log "DbErr" e) :: m.errors } |> pure

    SaveBookings cid bookings ->
      case bookings of
        [] ->
          ( m , getCustomerById cid )
        hd :: tl ->
          ( m , saveBooking (SaveBookings cid tl |> DbMsg) cid hd )

error : String -> Http.Error -> Msg
error action err =
  { action = action
  , http_err = Just err
  } |> Error |> DbMsg

error_ : String -> Msg
error_ action =
  { action = action
  , http_err = Nothing
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
            Ok  _ -> DbMsg <| SaveBookings id c.bookings
    in
        patchJson uri json C.jsonDecoder
        |> Http.send return

createCustomer : Customer -> Cmd Msg
createCustomer c =
    let json = C.jsonEncode c
        return res =
          case res of
            Err e -> error "createCustomer" e
            -- TODO: Save Bookings
            Ok c_ ->
              case c_.customer_id of
                Nothing -> error_ "createCustomerResponseID"
                Just id -> DbMsg <| SaveBookings id c.bookings
    in
        postJson res.customers json C.jsonDecoder
        |> Http.send return

saveBooking : Msg -> Int -> Booking -> Cmd Msg
saveBooking success customer_id b =
  let b_ = { b | customer_id = Just customer_id }
  in case b.booking_id of
      Nothing -> createBooking success b_
      Just id -> patchBooking  success b_ id

patchBooking : Msg -> Booking -> Int -> Cmd Msg
patchBooking success b id =
    let params = [ "booking_id=eq." ++ (toString id) ]
        uri = buildUri res.bookings params Nothing
        json = B.encode b
        return res =
          case res of
            Err e -> error "patchBooking" e
            Ok  _ -> success
    in
        patchJson uri json B.decode
        |> Http.send return

createBooking : Msg -> Booking -> Cmd Msg
createBooking success b =
    let json = B.encode b
        return res =
          case res of
            Err e -> error "createBooking" e
            Ok  _ -> success
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

postgrestFullResponseHeader =
  Http.header "Prefer" "return=representation"
postgrestSingleResposeHeader =
  Http.header "Accept" "application/vnd.pgrst.object+json"


patchJson : String -> Encode.Value -> Decode.Decoder a -> Http.Request a
patchJson uri json decoder =
  Http.request
    { method = "PATCH"
    , headers = [postgrestFullResponseHeader, postgrestSingleResposeHeader]
    , url = uri
    , body = Http.jsonBody json
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }

postJson : String -> Encode.Value -> Decode.Decoder a -> Http.Request a
postJson uri json decoder =
   Http.request
       { method = "POST"
       , headers = [postgrestFullResponseHeader, postgrestSingleResposeHeader]
       , url = uri
       , body = Http.jsonBody json
       , expect = Http.expectJson decoder
       , timeout = Nothing
       , withCredentials = False
       }
