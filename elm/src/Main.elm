module Main exposing (..)

import Html exposing (Html)
import Pure exposing (..)
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
  { customerId : Maybe Int
  , customerForm : CForm.Model
  }

init : Int -> (Model, Cmd Msg)
init id =
  ( Model Nothing (CForm.initEmpty ())
  , CHttp.getLatest CustomerReceived () )


-- UPDATE

type Msg =
    CustomerFormMsg CForm.Msg
  | Save
  | SaveResponse ( Result Http.Error ())
  | Previous
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
          case model.customerId of
            Just i ->
              ( model, CHttp.saveNewVersion SaveResponse c i )
            Nothing ->
              -- TODO: Insert new customer
              ( model, Cmd.none )

        Nothing ->
          -- Invalid state in form
          ( model, Cmd.none )

    SaveResponse (Ok ()) ->
      -- TODO: Refetch? If Server trims data, yes!
      ( model, Cmd.none )

    Previous ->
      case model.customerId of
        Just i ->
          ( model, CHttp.getPrevById CustomerReceived i )
        Nothing ->
          ( model, CHttp.getLatest CustomerReceived () )

    Next ->
      case model.customerId of
        Just i ->
          ( model, CHttp.getNextById CustomerReceived i )
        Nothing ->
          ( model, Cmd.none )

    CustomerReceived (Ok c) ->
      let model_ =
        { model
        | customerForm = CForm.init c
        , customerId = c.customer_id
        }
      in
      ( model_ , Cmd.none )

    -- TODO: Introduce error handling
    CustomerReceived (Err _) ->
      let model_ =
        { model
        | customerForm = CForm.initEmpty ()
        , customerId = Nothing
        }
      in
      ( model_ , Cmd.none )

    SaveResponse (Err _) ->
      ( model , Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
  div []
  [ button [onClick Previous] [text "Previous"]
  , button [onClick Next] [text "Next"]
  , button [onClick Save] [text "Save"]
  , CForm.view CustomerFormMsg model.customerForm
  ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


