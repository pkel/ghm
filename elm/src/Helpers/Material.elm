module Helpers.Material exposing (..)

import Material.Helpers exposing (Update)

liftCallback : (model -> submodel) --get
   -> (model -> submodel -> model)  -- set
   -> (subaction -> submodel -> (submodel, Cmd action))  -- update
   -> subaction -- action
   -> model -- model
   -> ( model, Cmd action )
liftCallback get set update action model =
    let
        ( submodel_, e ) =
            update action (get model)
    in
        ( set model submodel_, e )
