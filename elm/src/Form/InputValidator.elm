module BufferedInput exposing
    ( empty
    , set
    , get
    , valid
    , parse
    , init
    , Buffer
    , Spec
    , updater
    , Updater
    , update
    , maybe
    , check
    , date
    , string
    , float
    , int
    )

import Dict exposing (Dict)

import Date exposing (Date)
import Date.Format

import String exposing (trim)

type alias R a = Result String a

type alias Buffer = Dict String String

type alias Updater = String -> Buffer -> Buffer

type alias Init  a = a -> String
type alias Parse a = String -> R a

type alias TypeSpec a =
    ( Parse a, Init a )

type alias Spec a =
    { key      : String
    , hint     : String
    , label    : String
    , typeSpec : TypeSpec a
    }

updater : Spec a -> Updater
updater spec =
    set spec

update : Updater -> String -> Buffer -> Buffer
update f = f

empty : Buffer
empty =
    Dict.empty

set : Spec a -> String -> Buffer -> Buffer
set spec val buffer =
    let f x = Just val
    in
        Dict.update spec.key f buffer

get : Spec a -> Buffer -> String
get spec buffer =
    Dict.get spec.key buffer
    |> Maybe.withDefault ""

init : Spec a -> a -> Buffer -> Buffer
init spec val =
    val
    |> Tuple.second spec.typeSpec
    |> set spec

parse : Spec a -> Buffer -> R a
parse spec buf =
    get spec buf
    |> Tuple.first spec.typeSpec

valid : Spec a -> Buffer -> Bool
valid spec buf =
    case parse spec buf of
        Err _ -> False
        Ok  _ -> True


-- Parse / Init

maybe_p : Parse a -> Parse (Maybe a)
maybe_p f s =
    case trim s of
        ""  -> Ok Nothing
        str -> Result.map Just (f str)

maybe_i : Init a -> Init (Maybe a)
maybe_i f x =
    Maybe.map f x |> Maybe.withDefault ""

maybe : TypeSpec a -> TypeSpec (Maybe a)
maybe (parse, init) =
    (maybe_p parse, maybe_i init)


check_p : (a -> Bool) -> Parse a -> Parse a
check_p valid parse str =
    parse str
    |> Result.andThen (\x -> if valid x then Ok x else Err "check failed")

check : (a -> Bool) -> TypeSpec a -> TypeSpec a
check valid (parse, init) =
    (check_p valid parse, init)

string_p : Parse String
string_p s =
    trim s |> Ok

string_i : Init String
string_i =
    String.trim

string : TypeSpec String
string =
    (string_p, string_i)

int_p : Parse Int
int_p s =
    trim s |> String.toInt |> Result.mapError (\x -> "not an integer")

int_i : Init Int
int_i =
    toString

int : TypeSpec Int
int =
    (int_p, int_i)

float_p : Parse Float
float_p s =
    trim s |> String.toFloat |> Result.mapError (\x -> "not a float")

float_i : Init Float
float_i =
    toString

float : TypeSpec Float
float =
    (float_p, float_i)

date_p : Parse Date
date_p s =
    trim s |> Date.fromString |> Result.mapError (\x -> "not a date")

date_i : Init Date
date_i =
    Date.Format.format "%Y-%m-%d"

date : TypeSpec Date
date =
    (date_p, date_i)

dateFormatHint : String
dateFormatHint = "1995-04-15"

