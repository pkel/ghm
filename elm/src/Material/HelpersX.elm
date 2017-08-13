module Material.HelpersX exposing (..)

import Material.Helpers exposing (effect, cmd)

import Task

type alias UpdateCallback supaction callbacks action model =
    callbacks -> action -> model -> ( model, Cmd supaction )

callback : supaction -> model -> ( model , Cmd supaction )
callback supaction =
    cmd supaction |> effect

liftCallback : callbacks
   -> (model -> submodel) -- get
   -> (model -> submodel -> model) -- set
   -> UpdateCallback action callbacks subaction submodel
   -> subaction
   -> model
   -> ( model, Cmd action )
liftCallback callbacks get set update subaction model =
    let
        ( submodel_, e ) =
            update callbacks subaction (get model)
    in
        ( set model submodel_, e )
