module Plz exposing
    ( get )

import Dict exposing (Dict)
import Json.Decode as Decode
import PlzData

get : String -> Maybe String
get code =
    Dict.get code dict

dict : Dict String String
dict =
    let f acc codes cities =
            case (codes, cities) of
                ([] , _ ) -> acc
                (_  , []) -> acc
                (a :: aa , b :: bb) ->
                    f (Dict.insert a b acc) aa bb
    in
        f Dict.empty
            (decode PlzData.codes)
            (decode PlzData.cities)

decode : String -> List String
decode s =
    Decode.decodeString listDecoder s
    |> \r -> case r of
        Err x -> []
        Ok l -> l

listDecoder : Decode.Decoder (List String)
listDecoder =
    Decode.list Decode.string
