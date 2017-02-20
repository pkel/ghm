module Helpers.Date exposing
    ( print
    , short
    )

import Date exposing (..)
import Date.Format exposing (..)

short : Date -> String
short d =
    format "%d.%m.%y" d

-- TODO: used ?
print : Date -> String
print date =
     toString (day date) ++
     ". " ++
     toString (month date) ++
     " " ++
     toString (year date)
