module Main exposing (..)

import Html exposing (Html)
import Pure exposing (..)
import Buttons
import Html.Attributes  exposing (..)
import Html.Events exposing (..)


import Customer as C exposing (Customer)
import Booking  as B exposing (Booking)

import Helpers.Date as DateH

import CustomerForm as CForm
import BookingForm  as BForm
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
  , customer : Customer
  , customerForm : CForm.Model
  , filter : String
  , bookingForm : BForm.Model
  , focusedBooking : Maybe Booking
  }

init : (Model, Cmd Msg)
init =
  ( Model
        Nothing
        (C.empty ())
        (CForm.initEmpty ())
        ""
        (BForm.initEmpty ())
        Nothing
  , Db.getLatestCustomer CustomerReceived "" )


-- UPDATE

type Msg
    = CustomerFormMsg CForm.Msg
    | BookingFormMsg  BForm.Msg
    | Save
    | New
    | Previous
    | Next
    | Last
    | FilterChanged String
    | CustomerReceived (Result Http.Error Customer)
    | SelectBooking Booking

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CustomerFormMsg msg ->
      ( { model | customerForm = CForm.update msg model.customerForm } , Cmd.none )

    BookingFormMsg msg ->
      ( { model | bookingForm = BForm.update msg model.bookingForm } , Cmd.none )

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
      let model_ =
              { model
              | customer = c
--              , customerForm = CForm.init c
              , customerId = c.customer_id
              , focusedBooking = List.head c.bookings
              }
      in
      ( model_ , Cmd.none )

    CustomerReceived (Err _) ->
      ( model , Cmd.none )

    SelectBooking booking->
        ( { model | focusedBooking = Just booking } , Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    let customerDetail = C.view model.customer

        f b = div [onClick (SelectBooking b)] [B.viewSummary b]

        bookings = List.map f model.customer.bookings

        booking = case model.focusedBooking of
            Nothing -> []
            Just b -> [B.view b]

        data =
            Pure.group2 24
                [ ([customerDetail], 7)
                , ( bookings, 6)
                , ( booking, 11)
                ]
    in
        div [] [controls model, data]

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


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


