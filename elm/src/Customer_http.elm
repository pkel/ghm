module Customer_http exposing
  ( getById
  , getPrevById
  , getNextById
  , getLatest
  , saveNewVersion
  )

import Http
import Json.Encode as Encode

import Config
import Customer_t as CType exposing (Customer)

baseUrl : String
baseUrl = Config.apiUrl ++ "/customer_data"

getById : (Result Http.Error Customer -> msg) -> Int -> Cmd msg
getById encap id =
  let url
      = baseUrl
      ++ "?customer_id=eq."
      ++ (toString id)
      ++ "&order=customer_data_id.desc"
      ++ "&limit=1"
  in
  Http.send encap (Http.get url CType.jsonDecoderFirst)

getPrevById : (Result Http.Error Customer -> msg) -> String -> Int -> Cmd msg
getPrevById encap filter id =
  let url
      = baseUrl
      ++ "?customer_id=lt."
      ++ (toString id)
      ++ "&order=customer_id.desc,customer_data_id.desc"
      ++ "&limit=1"
  in
  let url_ = appendMaybeFilter url filter in
  Http.send encap (Http.get url_ CType.jsonDecoderFirst)

appendMaybeFilter : String -> String -> String
appendMaybeFilter url filter =
  case String.trim filter of
    "" -> url
    str -> url ++ "&keyword=ilike.%" ++ str ++ "%"

getNextById : (Result Http.Error Customer -> msg) -> String -> Int -> Cmd msg
getNextById encap filter id =
  let url
      = baseUrl
      ++ "?customer_id=gt."
      ++ (toString id)
      ++ "&order=customer_id.asc,customer_data_id.desc"
      ++ "&limit=1"
  in
  let url_ = appendMaybeFilter url filter in
  Http.send encap (Http.get url_ CType.jsonDecoderFirst)

getLatest : (Result Http.Error Customer -> msg) -> String -> Cmd msg
getLatest encap filter =
  let url
      = baseUrl
      ++ "?order=customer_id.desc,customer_data_id.desc"
      ++ "&limit=1"
  in
  let url_ = appendMaybeFilter url filter in
  Http.send encap (Http.get url_ CType.jsonDecoderFirst)

saveNewVersion : (Result Http.Error () -> msg) -> Customer -> Int -> Cmd msg
saveNewVersion encap c id =
  let cc = { c | customer_id = Just id } in
  let json = CType.jsonEncode cc in
  Http.send encap (postJsonExpectEmptyResponse baseUrl json)

-- TODO: separate this into helper module
postJsonExpectEmptyResponse : String -> Encode.Value -> Http.Request ()
postJsonExpectEmptyResponse url json =
  Http.request
    { method = "POST"
    , headers = []
    , url = url
    , body = Http.jsonBody json
    , expect = Http.expectStringResponse (\_ -> Ok ())
    , timeout = Nothing
    , withCredentials = False
    }
