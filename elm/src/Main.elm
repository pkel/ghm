module Main exposing (..)

import Html exposing (Html, text, br, strong)
import Html.Attributes as Attributes

import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Icon as Icon
import Material.Elevation as Elevation
import Material.Grid as Grid exposing (grid, cell, size, Device(..))
import Material.Layout as Layout
import Material.Options as Options
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Typography as Typography
import Material.Helpers exposing (pure, effect)
import Material.HelpersX exposing (liftCallback)

import Customer as C exposing (Customer)
import Booking  as B exposing (Booking)

import Defaults exposing (..)

import Array exposing (Array)
import Helpers.Array as ArrayX

import Cards.Note as NoteCard
import Cards.Customer as CustomerCard
import Cards.Individuals

import Date.Format as DateF

import Database as Db
import Http

import Debug exposing (log)

main =
  Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }


-- MODEL

type alias Mdl = Material.Model

type alias Model =
  { customerId : Maybe Int
  , customer : Customer
  , bookings : Array Booking
  , filter : String
  , focusedBooking : Int
  , customerCard : CustomerCard.Model
  , customerNoteCard : NoteCard.Model
  , individualsCard : Cards.Individuals.Model
  , bookingNoteCard : NoteCard.Model
  , mdl : Mdl
  }

empty : Model
empty =
    { customerId = Nothing
    , customer = C.empty
    , bookings = Array.empty
    , filter = ""
    , customerCard = CustomerCard.init C.empty
    , customerNoteCard = NoteCard.init ""
    , bookingNoteCard  = NoteCard.init ""
    , individualsCard  = Cards.Individuals.init []
    , focusedBooking   = -1
    , mdl = Material.model
    }

init : (Model, Cmd Msg)
init =
  ( empty
  , Db.getLatestCustomer CustomerReceived "" )


-- UPDATE

type Msg
    = New
    | Previous
    | Next
    | Last
    | FilterChanged String

    | CustomerReceived (Result Http.Error Customer)

    | SelectBooking Int

    | Ignore

    | UpdatedCustomer Customer
    | UpdatedCustomerNote String
    | UpdatedBookingNote  String
    | UpdatedIndividuals (List B.Individual)

    -- Pass through
    | CustomerCardMsg     (CustomerCard.Msg Msg)
    | CustomerNoteCardMsg (NoteCard.Msg Msg)
    | BookingNoteCardMsg  (NoteCard.Msg Msg)
    | IndividualsCardMsg  (Cards.Individuals.Msg Msg)

    -- Material Boilerplate
    | Mdl (Material.Msg Msg)


-- TODO: These two functions should clean up the interface
selectBooking : Int -> Model -> Model
selectBooking i model =
    case Array.get i model.bookings of
        Nothing -> model
        Just booking ->
            { model
            | focusedBooking  = i
            , individualsCard = Cards.Individuals.init booking.individuals
            , bookingNoteCard = NoteCard.init booking.note
            }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    New ->
        { model
        | customerId = Nothing
        , filter = ""
        } |> pure

    Previous ->
        model |> case model.customerId of
          Nothing -> pure
          Just i  -> effect <|
              Db.getPrevCustomerById CustomerReceived model.filter i

    Next ->
        model |> case model.customerId of
          Nothing -> pure
          Just i  -> effect <|
              Db.getNextCustomerById CustomerReceived model.filter i

    Last ->
        model |> effect (Db.getLatestCustomer CustomerReceived model.filter)

    FilterChanged str ->
        { model | filter = str } |>
        effect (Db.getLatestCustomer CustomerReceived str)

    CustomerReceived (Ok c) ->
        let b  = Array.fromList c.bookings
            c_ = { c | bookings = [] }
        in
            { model
            | customer = c_
            , customerId = c.customer_id
            , customerCard = CustomerCard.init c
            , customerNoteCard = NoteCard.init c.note
            , bookings = b
            }
            |> selectBooking 0
            |> pure

    CustomerReceived (Err _) ->
        -- TODO: notify
        pure model

    SelectBooking i ->
        selectBooking i model |> pure

    Ignore ->
        -- TODO: Where is this needed?
        pure model

    UpdatedCustomer c ->
        pure { model | customer = c }
        -- TODO: Save stuff to server

    UpdatedCustomerNote str ->
        let mod c = { c | note = str }
        in
            pure { model | customer = mod model.customer }
            -- TODO: Save stuff to server

    UpdatedBookingNote str ->
        let mod b = { b | note = str }
            bookings_ =
                ArrayX.modify model.focusedBooking mod model.bookings
        in
            pure { model | bookings = bookings_ }
            -- TODO: Save stuff to server

    UpdatedIndividuals lst ->
        let mod b =
                { b | individuals = lst }
            bookings_ =
                ArrayX.modify model.focusedBooking mod model.bookings
        in
            pure { model | bookings = bookings_ }
            -- TODO: Save stuff to server

    CustomerCardMsg msg_ ->
        liftCallback
            { delete = Ignore -- TODO: implement customer deletion
            , updated = UpdatedCustomer
            , mdl = Mdl
            }             .customerCard
            (\m x -> { m | customerCard = x })
            CustomerCard.update
            msg_ model

    CustomerNoteCardMsg msg_ ->
        liftCallback
            { mdl = Mdl
            , updated = UpdatedCustomerNote
            }             .customerNoteCard
            (\m x -> { m | customerNoteCard = x })
            NoteCard.update
            msg_ model

    IndividualsCardMsg msg_ ->
        liftCallback
            { mdl = Mdl
            , updated = UpdatedIndividuals
            }             .individualsCard
            (\m x -> { m | individualsCard = x })
            Cards.Individuals.update
            msg_ model

    BookingNoteCardMsg msg_ ->
        liftCallback
            { mdl = Mdl
            , updated = UpdatedBookingNote
            }             .bookingNoteCard
            (\m x -> { m | bookingNoteCard = x })
            NoteCard.update
            msg_ model

    Mdl msg_ -> Material.update Mdl msg_ model


-- CARDS

-- TODO: remove this hack as soon as all cards have their own module
defaultButton = Defaults.defaultButton Mdl

bookingSelectionCard : Mdl -> (Int -> Msg) -> Array Booking
          -> Int -> Html Msg
bookingSelectionCard mdl select bookings focused =
    let bookingsLst = Array.toList bookings
        summaries = List.map B.summary bookingsLst
        indeces = List.range 0 (Array.length bookings - 1)

        date d = Maybe.withDefault "" (Maybe.map (DateF.format "%d.%m.%y") d)
        range f t = text (date f ++ " bis " ++ date t)
        int  i = text (toString i)

        c = Options.css "text-align" "center"

        same index booking =
            Array.get focused bookings
                |> Maybe.map ((==) booking)
                |> Maybe.withDefault False

        row index booking summary =
            Table.tr
                [ Options.onClick (select index)
                , Table.selected
                    |> Options.when (same focused booking)
                ]
                [ Table.td [c] [range summary.from summary.to]
                , Table.td [c] [int summary.n_beds]
                , Table.td [c] [int summary.n_rooms]
                ]

        table =
            Table.table []
                [ Table.thead []
                    [ Table.tr []
                        [ Table.th [c] [Icon.i "date_range"]
                        , Table.th [c] [Icon.i "hotel"]
                        , Table.th [c] [Icon.i "vpn_key"]
                        ]
                    ]
                , Table.tbody [] (List.map3 row indeces bookingsLst summaries)
                ]

        actions =
            [ defaultButton mdl [30] "add" Ignore ]
    in
        Card.view
            [ defaultCard ]
            [ Card.title [ defaultCardTitle ] [ text "Buchungen" ]
            , Card.title [ Options.center ] [ table ]
            , Card.actions [ defaultActions ] actions
            ]

bookingCard : Booking -> Html Msg
bookingCard booking =
    Card.view
        [ defaultCard ]
        [ Card.title [ defaultCardTitle] [ text "Buchung" ]
        , Card.actions [] []
        ]

roomCard : B.Room -> Html Msg
roomCard room =
    Card.view
        [ defaultCard ]
        [ Card.title [ defaultCardTitle ] [ text "Zimmer" ]
        , Card.actions [] []
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

customerNoteCfg : NoteCard.Cfg Msg
customerNoteCfg =
    { lift  = CustomerNoteCardMsg
    , index = [800]
    , title = "Kundennotiz"
    }

bookingNoteCfg : NoteCard.Cfg Msg
bookingNoteCfg =
    { lift  = BookingNoteCardMsg
    , index = [801]
    , title = "Buchungsnotiz"
    }

customerCfg: CustomerCard.Cfg Msg
customerCfg =
    { lift       = CustomerCardMsg
    , index      = [802]
    }

individualsCfg : Cards.Individuals.Cfg Msg
individualsCfg =
    { lift = IndividualsCardMsg
    , index = [803]
    }


viewBody : Model -> Html Msg
viewBody model =
    let -- TODO: Helpers.Maybe
        maybeMapDefault default f x = Maybe.map f x |> Maybe.withDefault default

        -- customer data

        customer = CustomerCard.view
            customerCfg
            model.mdl
            model.customerCard

        -- customer note

        customerNote = NoteCard.view
            customerNoteCfg
            model.mdl
            model.customerNoteCard

        -- booking list with booking selection

        selection = bookingSelectionCard
            model.mdl
            SelectBooking
            model.bookings
            model.focusedBooking

        -- list of individuals, editable

        individuals = Cards.Individuals.view
            individualsCfg
            model.mdl
            model.individualsCard

        -- booking note

        bookingNote = NoteCard.view
            bookingNoteCfg
            model.mdl
            model.bookingNoteCard
    in
        grid
            [ Grid.noSpacing
            ]
            [ cell [ size All 4 ] [ customer, customerNote, selection ]
            , cell [ size All 4 ] [ individuals ]
            , cell [ size All 4 ] [ bookingNote ]
            ]

controls : Model -> Html Msg
controls model =
    let filter =
            Textfield.render Mdl [0] model.mdl
                [ Textfield.label "Suche"
                , Textfield.text_
                , Textfield.value model.filter
                , Options.onInput (FilterChanged)
                ] []

        filterIcon = Icon.view "search"
            [ Options.css "margin-right" "5px" ]

        btn i action icon = defaultButton model.mdl i icon action
    in
        Layout.row
            [ Color.background Color.accent
            , Color.text Color.primary
            ]
            [ filterIcon
            , filter
            , Layout.spacer
            , btn [1] Previous "chevron_left"
            , btn [2] Next "chevron_right"
            , btn [3] Last "last_page"
            , Layout.spacer
            , btn [4] New "library_add"
            ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


