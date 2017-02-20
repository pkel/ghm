module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)

import Material
import Material.Button as Button
import Material.Card as Card
import Material.Icon as Icon
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Layout as Layout
import Material.Options as Options
import Material.Textfield as Textfield

import Customer as C exposing (Customer)
import Booking  as B exposing (Booking)

import Database as Db
import Http


main =
  Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions }

-- MODEL

type alias Mdl = Material.Model

type alias Model =
  { customerId : Maybe Int
  , customer : Customer
  , filter : String
  , focusedBooking : Maybe Booking
  , mdl : Mdl
  }

init : (Model, Cmd Msg)
init =
  ( Model
        Nothing
        (C.empty ())
        ""
        Nothing
        Material.model
  , Db.getLatestCustomer CustomerReceived "" )


-- UPDATE

type Msg
    = New
    | Previous
    | Next
    | Last
    | FilterChanged String
    | CustomerReceived (Result Http.Error Customer)
    | SelectBooking Booking
    | Mdl (Material.Msg Msg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    New ->
      let model_ =
        { model
        | customerId = Nothing
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
              , customerId = c.customer_id
              , focusedBooking = List.head c.bookings
              }
      in
      ( model_ , Cmd.none )

    CustomerReceived (Err _) ->
      ( model , Cmd.none )

    SelectBooking booking ->
        ( { model | focusedBooking = Just booking } , Cmd.none )

    Mdl msg_ -> Material.update Mdl msg_ model


-- CARDS

customerCard : Customer -> Html Msg
customerCard c =
    Card.view
        [ ]
        [ Card.title [] [ text c.keyword ]
        , Card.text [] [ C.view c ]
        , Card.actions [ Card.border ] [ text "not yet"]
        ]

bookingSummaryCard : Booking -> (Booking -> Msg) -> Html Msg
bookingSummaryCard b action =
    let s = B.summary b
    in
        Card.view
            [ Options.onClick (action b)
            ]
            [ Card.title [] [ text "Buchung" ]
            , Card.text [] [ B.viewSummary b ]
            ]

-- VIEW

view : Model -> Html Msg
view model =
    Layout.render Mdl model.mdl [ Layout.fixedHeader ]
        { header = [ controls model ]
        , drawer = []
        , tabs = ( [], [] )
        , main = [ viewBody model ]
        }

viewBody : Model -> Html Msg
viewBody model =
    let customer = customerCard model.customer

        f b = bookingSummaryCard b SelectBooking

        bookings = List.map f model.customer.bookings

        booking = case model.focusedBooking of
            Nothing -> []
            Just b -> [B.view b]
    in
        grid []
            [ cell [ size All 4 ] [ customer ]
            , cell [ size All 4 ] bookings
            ]

controls : Model -> Html Msg
controls model =
    let filter =
            -- Pure.textfield "Suche" FilterChanged model.filter
            Textfield.render Mdl [0] model.mdl
                [ Textfield.label "Suche"
                , Textfield.text_
                , Textfield.value model.filter
                , Options.onInput (FilterChanged)
                ] []

        filterIcon =
            Icon.i "search"

        btn action icon =
            Button.render Mdl [0] model.mdl
                [ Button.icon
                , Options.onClick action
                ]
                [ Icon.i icon]
    in
        Layout.row []
            [ filter
            , Layout.spacer
            , btn Previous "chevron_left"
            , btn Next "chevron_right"
            , btn Last "last_page"
            , Layout.spacer
            , btn New "library_add"
            ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


