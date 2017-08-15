module Material.Style exposing (..)

import Material.Options as O

padding0 = O.css "padding" "0"
margin0  = O.css "margin" "0"

marginVert0 = O.many [ O.css "margin-bottom" "0", O.css "margin-top" "0" ]

box0 = O.many [ padding0, margin0 ]

widthMax = O.css "width" "100%"
widthAuto = O.css "width" "auto"
