module Helpers.Date exposing
    ( print
    )

import Date exposing (..)

print : Date -> String
print date =
     toString (day date) ++
     ". " ++
     toString (month date) ++
     " " ++
     toString (year date)
