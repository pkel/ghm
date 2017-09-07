module Form.Show exposing (..)

import Date exposing (Date)
import Date.Format as DateF

maybe : ( a -> String ) -> Maybe a -> String
maybe f x =
    Maybe.map f x |> Maybe.withDefault ""

int : Int -> String
int =
    toString

string : String -> String
string =
    String.trim

float : Float -> String
float =
    toString

date : Date -> String
date =
    DateF.format "%Y-%m-%d"

dateFormatHint : String
dateFormatHint = "1995-04-15"
