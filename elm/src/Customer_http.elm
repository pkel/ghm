module Customer_http exposing
  ( getById
  , getPrevById
  , getNextById
  , getLatest
  , save
  , new
  )

import Http
import Json.Encode as Encode
import Json.Decode as Decode

import Config
import Customer_t as CType exposing (Customer)

resourceUri : String
resourceUri = Config.apiUrl ++ "/customers"

getById : (Result Http.Error Customer -> msg) -> Int -> Cmd msg
getById encap id =
  let uri
      = resourceUri
      ++ "?customer_id=eq."
      ++ (toString id)
  in
  Http.send encap (Http.get uri CType.jsonDecoderFirst)

getPrevById : (Result Http.Error Customer -> msg) -> String -> Int -> Cmd msg
getPrevById encap filter id =
  let uri
      = resourceUri
      ++ "?customer_id=lt."
      ++ (toString id)
      ++ "&order=customer_id.desc"
      ++ "&limit=1"
  in
  let uri_ = appendMaybeFilter uri filter in
  Http.send encap (Http.get uri_ CType.jsonDecoderFirst)

appendMaybeFilter : String -> String -> String
appendMaybeFilter uri filter =
  case String.trim filter of
    "" -> uri
    str ->
        let str_ = Http.encodeUri ("%" ++ str ++ "%")
        in uri ++ "&keyword=ilike." ++ str_

getNextById : (Result Http.Error Customer -> msg) -> String -> Int -> Cmd msg
getNextById encap filter id =
  let uri
      = resourceUri
      ++ "?customer_id=gt."
      ++ (toString id)
      ++ "&order=customer_id.asc"
      ++ "&limit=1"
  in
  let uri_ = appendMaybeFilter uri filter in
  Http.send encap (Http.get uri_ CType.jsonDecoderFirst)

getLatest : (Result Http.Error Customer -> msg) -> String -> Cmd msg
getLatest encap filter =
  let uri
      = resourceUri
      ++ "?order=customer_id.desc"
      ++ "&limit=1"
  in
  let uri_ = appendMaybeFilter uri filter in
  Http.send encap (Http.get uri_ CType.jsonDecoderFirst)

save : (Result Http.Error Customer -> msg) -> Customer -> Int -> Cmd msg
save encap c id =
  let uri
      = resourceUri
      ++ "?customer_id=eq."
      ++ (toString id)
  in
  let cc = { c | customer_id = Just id } in
  let json = CType.jsonEncode cc in
  Http.send encap (patchJson uri json CType.jsonDecoderFirst)

new : (Result Http.Error Customer -> msg) -> Customer -> Cmd msg
new encap c =
  let cc = { c | customer_id = Nothing } in
  let json = CType.jsonEncode cc in
  Http.send encap (postJson resourceUri json CType.jsonDecoder)


-- TODO: separate this into (postgrest) helper module
emptyJson : Encode.Value
emptyJson =
  Encode.object []

emptyJsonList : Encode.Value
emptyJsonList =
  Encode.list []

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

