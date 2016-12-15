module Main exposing (..)

import Html exposing (Html)
import Pure exposing (..)
import Icons
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
  , filter : String
  }

init : Int -> (Model, Cmd Msg)
init id =
  ( Model Nothing (CForm.initEmpty ()) ""
  , CHttp.getLatest CustomerReceived "" )


-- UPDATE

type Msg =
    CustomerFormMsg CForm.Msg
  | Save
  | SaveResponse ( Result Http.Error ())
  | Previous
  | Next
  | FilterChanged String
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
          ( model, CHttp.getPrevById CustomerReceived model.filter i)
        Nothing ->
          ( model, CHttp.getLatest CustomerReceived model.filter)

    Next ->
      case model.customerId of
        Just i ->
          ( model, CHttp.getNextById CustomerReceived model.filter i)
        Nothing ->
          ( model, Cmd.none )

    FilterChanged str ->
      ( { model | filter = str }, CHttp.getLatest CustomerReceived str )

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
  [ controls model
  , CForm.view CustomerFormMsg model.customerForm
  ]

controls : Model -> Html Msg
controls model =
  let left =
    Pure.textfield "Suche" FilterChanged model.filter
  in
  let middle =
    div [style [("text-align","center")]]
        [ button [onClick Previous] [Icons.prev]
        , button [onClick Next]     [Icons.next]
        ]
  in
  let right =
    div [style [("text-align","right")]]
        [ button [onClick Save] [Icons.save]
        ]
  in
  let bar =
    Pure.group2 24
        [ ([left], 7)
        , ([], 1)
        , ([middle], 7)
        , ([], 1)
        , ([right], 8)
        ]
  in
  Pure.form [] [bar]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


