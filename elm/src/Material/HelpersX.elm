module Material.HelpersX exposing (..)

import Material.Helpers exposing (Update, effect)

import Task

callback : ( a -> msg ) -> a -> model -> ( model , Cmd msg )
callback f msg =
    effect (Task.perform f (Task.succeed msg))

liftCallback : (model -> submodel) -- get
   -> (model -> submodel -> model) -- set
   -> (subaction -> submodel -> (submodel, Cmd action)) -- update
   -> subaction -- action
   -> model -- model
   -> ( model, Cmd action )
liftCallback get set update subaction model =
    let
        ( submodel_, e ) =
            update subaction (get model)
    in
        ( set model submodel_, e )
