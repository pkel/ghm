module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Customer_t exposing (Customer)

import Customer_t as CType
import Customer_form as CForm
import Customer_http as CHttp
import Http


main =
  Html.program
  { init = init 1151
  , view = view
  , update = update
  , subscriptions = subscriptions }

-- MODEL

type alias Model =
  { customerId : Int
  , customerForm : CForm.Model
  }

init : Int -> (Model, Cmd Msg)
init id =
  ( Model id (CType.empty ())
  , CHttp.getById CustomerReceived id )


-- UPDATE

type Msg =
    CustomerFormMsg CForm.Msg
  | Save
  | SaveResponse ( Result Http.Error ())
  | Next
  | CustomerReceived (Result Http.Error Customer)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CustomerFormMsg msg ->
      ( { model | customerForm = CForm.update msg model.customerForm } , Cmd.none )

    Save ->
      -- trigger put/post
      case CForm.extract model.customerForm of
        Just c ->
          -- ( model, CHttp.save CustomerReceived c model.customerId )
          ( model, CHttp.saveNewVersion SaveResponse c model.customerId )

        Nothing ->
          -- Invalid state in form
          ( model, Cmd.none )

    SaveResponse result ->
      -- Refetch, in case of server side trimming
      case result of
        Ok () -> ( model, CHttp.getById CustomerReceived model.customerId )
        Err _ -> ( model , Cmd.none ) -- TODO: report this!

    Next ->
      -- incrementing the id is not the best approach here TODO: fix.
      let newId = model.customerId + 1 in
      ( { model | customerId = newId }, CHttp.getById CustomerReceived newId )

    CustomerReceived (Ok c) ->
      ( { model | customerForm =  c } , Cmd.none )

    CustomerReceived (Err _) ->
      ( { model | customerForm = CType.empty () } , Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
  div []
  [ button [onClick Next] [text "Next"]
  , button [onClick Save] [text "Save"]
  , CForm.view CustomerFormMsg model.customerForm
  ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


