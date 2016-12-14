module Customer_http exposing
  ( getById
  , getPrevById
  , getNextById
  , getLatest
  , saveNewVersion
  )

import Http
import Json.Encode as Encode

import Customer_t as CType exposing (Customer)

baseUrl : String
baseUrl = "http://localhost:3000/customer_data"

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

getPrevById : (Result Http.Error Customer -> msg) -> Int -> Cmd msg
getPrevById encap id =
  let url
      = baseUrl
      ++ "?customer_id=lt."
      ++ (toString id)
      ++ "&order=customer_id.desc,customer_data_id.desc"
      ++ "&limit=1"
  in
  Http.send encap (Http.get url CType.jsonDecoderFirst)

getNextById : (Result Http.Error Customer -> msg) -> Int -> Cmd msg
getNextById encap id =
  let url
      = baseUrl
      ++ "?customer_id=gt."
      ++ (toString id)
      ++ "&order=customer_id.asc,customer_data_id.desc"
      ++ "&limit=1"
  in
  Http.send encap (Http.get url CType.jsonDecoderFirst)

getLatest : (Result Http.Error Customer -> msg) -> () -> Cmd msg
getLatest encap () =
  let url
      = baseUrl
      ++ "?order=customer_id.desc,customer_data_id.desc"
      ++ "&limit=1"
  in
  Http.send encap (Http.get url CType.jsonDecoderFirst)

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
