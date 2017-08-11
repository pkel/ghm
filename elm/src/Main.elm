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

import Customer as C exposing (Customer)
import Booking  as B exposing (Booking)

import Defaults exposing (..)

import Array exposing (Array)
import Helpers.Array as ArrayX

import Cards.Note as NoteCard
import Cards.CustomerDetail as CustomerCard
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
  , subscriptions = subscriptions }


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

model : Model
model =
    { customerId = Nothing
    , customer = C.empty
    , bookings = Array.empty
    , filter = ""
    , customerCard = CustomerCard.show
    , customerNoteCard = NoteCard.show
    , bookingNoteCard = NoteCard.show
    , individualsCard = Cards.Individuals.model
    , focusedBooking = -1
    , mdl = Material.model
    }

init : (Model, Cmd Msg)
init =
  ( model
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

    | EditCustomer
    | EditCustomerDone
    | DeleteCustomer

    | EditCustomerNote
    | EditCustomerNoteDone
    | DeleteCustomerNote

    | EditBookingNote
    | EditBookingNoteDone
    | DeleteBookingNote

    | UpdatedIndividuals (List B.Individual)

    -- Pass through
    | CustomerCardMsg     CustomerCard.Msg
    | CustomerNoteCardMsg NoteCard.Msg
    | IndividualsCardMsg  (Cards.Individuals.Msg Msg)
    | BookingNoteCardMsg  NoteCard.Msg

    -- Material Boilerplate
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    New ->
        -- TODO
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
        let bookings = c.bookings
            c_ = { c | bookings = [] }
            model_ =
                { model
                | customer = c_
                , customerId = c.customer_id
                , bookings = Array.fromList bookings
                , customerNoteCard = NoteCard.show
                , bookingNoteCard = NoteCard.show
                , individualsCard = Cards.Individuals.model
                , focusedBooking = 0
                }
        in
            ( model_ , Cmd.none )

    CustomerReceived (Err _) ->
      ( model , Cmd.none )

    SelectBooking index ->
        ( { model | focusedBooking = index } , Cmd.none )

    Ignore ->
        (model, Cmd.none)

    EditCustomer ->
        let model_ =
                { model
                | customerCard = CustomerCard.edit model.customer
                }
        in
            (model_, Cmd.none)

    EditCustomerDone ->
        let customer_ =
                CustomerCard.extract model.customerCard model.customer
                    |> Maybe.withDefault model.customer

            model_ =
                { model
                | customerCard = CustomerCard.show
                , customer = customer_
                }
        in
            (model_, Cmd.none)

    DeleteCustomer ->
        -- TODO: Delete Customer does nothing
        (model, Cmd.none)

    DeleteCustomerNote ->
        let customer_ = C.setNote model.customer ""
            model_ =
                { model
                | customer = customer_
                , customerNoteCard = NoteCard.show
                }
        in
            ( model_ , Cmd.none )

    EditCustomerNote ->
        ( { model | customerNoteCard = NoteCard.edit model.customer.note }
        , Cmd.none )

    EditCustomerNoteDone ->
        let customer_ =
                NoteCard.extract model.customerNoteCard
                    |> Maybe.map (C.setNote model.customer)
                    |> Maybe.withDefault model.customer

            model_ =
                { model
                | customerNoteCard = NoteCard.show
                , customer = customer_
                }
        in
        ( model_ , Cmd.none )

    DeleteBookingNote ->
        let f b = { b | note = "" }

            bookings_ =
                ArrayX.modify model.focusedBooking f model.bookings

            model_ =
                { model
                | bookings = bookings_
                , bookingNoteCard = NoteCard.show
                }
        in
            (model_ , Cmd.none )

    EditBookingNote ->
        let noteCard_ =
                Array.get model.focusedBooking model.bookings
                    |> Maybe.map .note
                    |> Maybe.map NoteCard.edit
                    |> Maybe.withDefault NoteCard.show
        in
        ( { model | bookingNoteCard = noteCard_ }
        , Cmd.none )

    EditBookingNoteDone ->
        let setNote note_ booking =
                { booking | note = note_ }

            bookings_ =
                NoteCard.extract model.bookingNoteCard
                    |> Maybe.map (
                        \note ->
                        ArrayX.modify
                            model.focusedBooking
                            (setNote note)
                            model.bookings
                        )
                    |> Maybe.withDefault model.bookings

            model_ =
                { model
                | bookings = bookings_
                , bookingNoteCard = NoteCard.show
                }
        in
            (model_ , Cmd.none )

    UpdatedIndividuals lst ->
        let setInds b =
                { b | individuals = lst }

            _ = log "was" model.bookings
            _ = log "edited" lst
        in
        ArrayX.modify model.focusedBooking setInds model.bookings |>
        -- TODO: Save stuff to server
        \x -> ( { model | bookings = x } , Cmd.none)

    CustomerCardMsg msg_ ->
        let ( model_, cmd_) = CustomerCard.update msg_ model.customerCard
        in
            ( { model | customerCard = model_ }
            , Cmd.map CustomerCardMsg cmd_
            )

    CustomerNoteCardMsg msg_ ->
        let ( model_, cmd_) = NoteCard.update msg_ model.customerNoteCard
        in
            ( { model | customerNoteCard = model_ }
            , Cmd.map CustomerNoteCardMsg cmd_
            )

    IndividualsCardMsg msg_ ->
        let ( model_, cmd_) =
                Cards.Individuals.update msg_ model.individualsCard
        in
            ( { model | individualsCard = model_ }
            , cmd_
            )

    BookingNoteCardMsg msg_ ->
        let ( model_, cmd_) = NoteCard.update msg_ model.bookingNoteCard
        in
            ( { model | bookingNoteCard = model_ }
            , Cmd.map BookingNoteCardMsg cmd_
            )

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

viewBody : Model -> Html Msg
viewBody model =
    let mdl = model.mdl

        -- TODO: Helpers.Maybe
        maybeMapDefault default f x = Maybe.map f x |> Maybe.withDefault default

        -- customer data

        customerCfg =
            { mdl        = model.mdl
            , mdlMessage = Mdl
            , msg        = CustomerCardMsg
            , index      = [1]
            , delete     = DeleteCustomer
            , edit       = EditCustomer
            , done       = EditCustomerDone
            }

        customer = CustomerCard.view
            customerCfg
            model.customerCard
            model.customer

        -- customer note

        customerNoteCfg =
            { mdl        = model.mdl
            , mdlMessage = Mdl
            , msg        = CustomerNoteCardMsg
            , index      = [2]
            , title      = "Kundennotiz"
            , delete     = DeleteCustomerNote
            , edit       = EditCustomerNote
            , done       = EditCustomerNoteDone
            }

        customerNote =
            NoteCard.view
                customerNoteCfg
                model.customerNoteCard
                model.customer.note

        -- booking list with booking selection

        selection = bookingSelectionCard mdl SelectBooking
            model.bookings model.focusedBooking

        -- list of individuals, editable

        individualsCfg =
            { mdl     = model.mdl
            , mdlMsg  = Mdl
            , msg     = IndividualsCardMsg
            , index   = [3]
            , title   = "Gäste"
            -- TODO: Adapt NoteCards (and others) to fit this api
            , updated = UpdatedIndividuals
            }

        individuals booking = Cards.Individuals.view individualsCfg
            model.individualsCard booking.individuals

        -- cards related to selected booking (middle)

        bookingRelatedCardsL booking = List.concat
            [ [ bookingCard booking, individuals booking ]
            , List.map roomCard booking.rooms
            ]

        -- cards related to selected booking (right)

        bookingNoteCfg =
            { customerNoteCfg
            | msg = BookingNoteCardMsg
            , index = [4]
            , title = "Buchungsnotiz"
            , delete = DeleteBookingNote
            , edit = EditBookingNote
            , done = EditBookingNoteDone
            }

        bookingNote booking =
            NoteCard.view
                bookingNoteCfg
                model.bookingNoteCard
                booking.note


        bookingRelatedCardsR b = [ bookingNote b ]

        -- selected booking (may be none)

        booking =
            Array.get model.focusedBooking model.bookings

    in
        grid
            [ Grid.noSpacing
            ]
            [ cell [ size All 4 ] [ customer, customerNote, selection ]
            , cell [ size All 4 ]
                ( maybeMapDefault [] bookingRelatedCardsL booking)
            , cell [ size All 4 ]
                ( maybeMapDefault [] bookingRelatedCardsR booking)
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


