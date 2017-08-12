module Helpers.Material exposing (..)

import Material.Helpers exposing (Update)

liftCallback get set fwd update action model =
    let
        ( submodel_, e ) =
            update action (get model)
    in
        ( set model submodel_, e )
