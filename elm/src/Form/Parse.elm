module Form.Parse exposing (..)

import Date exposing (Date)
import Date.Format as DateF

import String exposing (trim)

type alias R a = Result () a

maybe : ( String -> R a ) -> String -> R (Maybe a)
maybe f s =
    case trim s of
        ""  -> Ok Nothing
        str -> Result.map Just (f str)

string : String -> R String
string s =
    trim s |> Ok

int : String -> R Int
int s =
    trim s |> String.toInt |> Result.mapError (\x -> ())

float : String -> R Float
float s =
    trim s |> String.toFloat |> Result.mapError (\x -> ())

date : String -> R Date
date s =
    trim s |> Date.fromString |> Result.mapError (\x -> ())
