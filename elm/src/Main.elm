module Main exposing (..)

import Html exposing (Html)
import Pure exposing (..)
import Buttons
import Table
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
  , customerForm : CForm.Model
  , filter : String
  , bookingsTableState : Table.State
  , bookings : List LocalBooking
  , bookingForm : BForm.Model
  , focusedBooking : Maybe Booking
  }

type alias LocalBooking =
    { id : Int
    , summary : B.Summary
    , data : B.Booking
    }

init : (Model, Cmd Msg)
init =
  ( Model
        Nothing
        (CForm.initEmpty ())
        ""
        (Table.initialSort "ID")
        []
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
    | BookingTableSetState Table.State
    | BookingTableClicked LocalBooking

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
      let f x pre =
              (LocalBooking (List.length pre + 1) (B.summary x)  x) :: pre
          bookings_ =
              List.foldl f [] c.bookings
          model_ =
              { model
              | customerForm = CForm.init c
              , customerId = c.customer_id
              , bookings = bookings_
              , focusedBooking = List.head c.bookings
              }
      in
      ( model_ , Cmd.none )

    CustomerReceived (Err _) ->
      ( model , Cmd.none )

    BookingTableSetState newState ->
        ( { model | bookingsTableState = newState } , Cmd.none )

    BookingTableClicked localBooking ->
        ( { model | focusedBooking = Just localBooking.data } , Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    let bookingDetail =
            case model.focusedBooking of
                Nothing -> div [] []
                Just b -> B.view b
    in
  div []
  [ controls model
  , CForm.view CustomerFormMsg model.customerForm
  , bookingsTable model
  , bookingDetail
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
            Maybe.withDefault "" (Maybe.map DateH.print a)
        fst = Tuple.first
        snd = Tuple.second
    in
    Table.customConfig
        { toId = \t -> toString t.id
        , toMsg = BookingTableSetState
        , columns =
            [ Table.intColumn "ID" .id
            -- TODO: Make this sorting correctly
            , Table.stringColumn "von" (\t -> viewMaybeDate t.summary.from)
            , Table.stringColumn "bis" (\t -> viewMaybeDate t.summary.to)
            , Table.intColumn "Zimmer" (\t -> t.summary.n_rooms)
            , Table.intColumn "Betten" (\t -> t.summary.n_beds)
            ]
        , customizations =
            { defaults
            | tableAttrs = [ class "pure-table" ]
            , rowAttrs = \t -> [ onClick (BookingTableClicked t) ]
            }
        }

bookingsTable : Model -> Html Msg
bookingsTable model =
    Table.view bookingsTableConfig model.bookingsTableState model.bookings


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


