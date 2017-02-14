module Main exposing (..)

import Html exposing (Html)
import Pure exposing (..)
import Buttons
import Table
import Html.Attributes  exposing (..)
import Html.Events exposing (..)


import Customer as C exposing (Customer)
import Booking  as B exposing (Booking)

import CustomerForm as CForm
import Database as Db
import Http


main =
  Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions }

-- MODEL

type alias Model =
  { customerId : Maybe Int
  , customerForm : CForm.Model
  , filter : String
  , bookingsTableState : Table.State
  , bookingSummary : List LocalBooking
  }

type alias LocalBooking = (Int, B.Summary)

init : (Model, Cmd Msg)
init =
  ( Model Nothing (CForm.initEmpty ()) "" (Table.initialSort "ID") []
  , Db.getLatestCustomer CustomerReceived "" )


-- UPDATE

type Msg =
    CustomerFormMsg CForm.Msg
  | Save
  | New
  | Previous
  | Next
  | Last
  | FilterChanged String
  | CustomerReceived (Result Http.Error Customer)
  | BookingsTableSetState Table.State

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
              ( model, Db.saveCustomer CustomerReceived c i )
            Nothing ->
              ( model, Db.newCustomer CustomerReceived c)

        Nothing ->
          -- Invalid state in form
          ( model, Cmd.none )

    New ->
      let model_ =
        { model
        | customerForm = CForm.initEmpty ()
        , customerId = Nothing
        , filter = ""
       }
      in
      ( model_ , Cmd.none )

    Previous ->
      case model.customerId of
        Just i ->
          ( model, Db.getPrevCustomerById CustomerReceived model.filter i)
        Nothing ->
          ( model, Db.getLatestCustomer CustomerReceived model.filter)

    Next ->
      case model.customerId of
        Just i ->
          ( model, Db.getNextCustomerById CustomerReceived model.filter i)
        Nothing ->
          ( model, Cmd.none )

    Last ->
      ( model, Db.getLatestCustomer CustomerReceived model.filter )

    FilterChanged str ->
      ( { model | filter = str }, Db.getLatestCustomer CustomerReceived str )

    CustomerReceived (Ok c) ->
      let f x pre =
              ((List.length pre + 1) , x) :: pre
          summaries  = List.map B.summary c.bookings
          summaries_ =
              List.foldl f [] summaries
          model_ =
              { model
              | customerForm = CForm.init c
              , customerId = c.customer_id
              , bookingSummary = summaries_
              }
      in
      ( model_ , Cmd.none )

    CustomerReceived (Err _) ->
      ( model , Cmd.none )

    BookingsTableSetState newState ->
        ( { model | bookingsTableState = newState } , Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
  div []
  [ controls model
  , CForm.view CustomerFormMsg model.customerForm
  , bookingsTable model
  ]

controls : Model -> Html Msg
controls model =
  let left =
    Pure.textfield "Suche" FilterChanged model.filter
  in
  let middle =
    div [style [("text-align","center")]]
        [ Buttons.prev Previous
        , Buttons.next Next
        , Buttons.last Last
        , Buttons.add  New
        ]
  in
  let right =
    div [style [("text-align","right")]]
        [ Buttons.save Save
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

bookingsTableConfig : Table.Config LocalBooking Msg
bookingsTableConfig =
    let defaults = Table.defaultCustomizations
        viewMaybeDate a =
            Maybe.withDefault "" (Maybe.map toString a)
        fst = Tuple.first
        snd = Tuple.second
    in
    Table.customConfig
        { toId = \t -> toString (fst t)
        , toMsg = BookingsTableSetState
        , columns =
            [ Table.intColumn "ID" fst
            , Table.stringColumn "von" (\t -> viewMaybeDate (snd t).from)
            , Table.stringColumn "bis" (\t -> viewMaybeDate (snd t).to)
            , Table.intColumn "Zimmer" (\t -> (snd t).n_rooms)
            , Table.intColumn "Betten" (\t -> (snd t).n_beds)
            ]
        , customizations =
            { defaults
            | tableAttrs = [ class "pure-table" ]
            }
        }

bookingsTable : Model -> Html Msg
bookingsTable model =
    Table.view bookingsTableConfig model.bookingsTableState model.bookingSummary


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


