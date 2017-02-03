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
customersUri = Config.apiUrl ++ "/customers"


---------------
-- Customers --
---------------

getCustomerById : (Result Http.Error Customer -> msg) -> Int -> Cmd msg
getCustomerById encap id =
  let uri
      = customersUri
      ++ "?customer_id=eq."
      ++ (toString id)
  in
  Http.send encap (Http.get uri C.jsonDecoderFirst)

getPrevCustomerById : (Result Http.Error Customer -> msg) -> String -> Int -> Cmd msg
getPrevCustomerById encap filter id =
  let uri
      = customersUri
      ++ "?customer_id=lt."
      ++ (toString id)
      ++ "&order=customer_id.desc"
      ++ "&limit=1"
  in
  let uri_ = appendKeywordFilter uri filter in
  Http.send encap (Http.get uri_ C.jsonDecoderFirst)

getNextCustomerById : (Result Http.Error Customer -> msg) -> String -> Int -> Cmd msg
getNextCustomerById encap filter id =
  let uri
      = customersUri
      ++ "?customer_id=gt."
      ++ (toString id)
      ++ "&order=customer_id.asc"
      ++ "&limit=1"
  in
  let uri_ = appendKeywordFilter uri filter in
  Http.send encap (Http.get uri_ C.jsonDecoderFirst)

getLatestCustomer : (Result Http.Error Customer -> msg) -> String -> Cmd msg
getLatestCustomer encap filter =
  let uri
      = customersUri
      ++ "?order=customer_id.desc"
      ++ "&limit=1"
  in
  let uri_ = appendKeywordFilter uri filter in
  Http.send encap (Http.get uri_ C.jsonDecoderFirst)

saveCustomer : (Result Http.Error Customer -> msg) -> Customer -> Int -> Cmd msg
saveCustomer encap c id =
  let uri
      = customersUri
      ++ "?customer_id=eq."
      ++ (toString id)
  in
  let cc = { c | customer_id = Just id } in
  let json = C.jsonEncode cc in
  Http.send encap (patchJson uri json C.jsonDecoderFirst)

newCustomer : (Result Http.Error Customer -> msg) -> Customer -> Cmd msg
newCustomer encap c =
  let cc = { c | customer_id = Nothing } in
  let json = C.jsonEncode cc in
  Http.send encap (postJson customersUri json C.jsonDecoder)


----------------------
-- Common Functions --
----------------------

appendKeywordFilter : String -> String -> String
appendKeywordFilter uri filter =
  case String.trim filter of
    "" -> uri
    str ->
        let str_ = Http.encodeUri ("%" ++ str ++ "%")
        in uri ++ "&keyword=ilike." ++ str_


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

