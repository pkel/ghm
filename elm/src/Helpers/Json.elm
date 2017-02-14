module Helpers.Json exposing (..)

import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder)

decodeDate : Decoder Date
decodeDate =
    Decode.string
        |> Decode.andThen (\val ->
        case Date.fromString val of
          Err err -> Decode.fail err
          Ok date -> Decode.succeed date)

-- TODO: Place first of list method from Customer.elm here.

