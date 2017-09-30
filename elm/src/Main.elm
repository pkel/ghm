module Main exposing (..)

import Html exposing (Html, text, br, strong)
import Html.Attributes as Attributes

import Material
import Material.Button as Button
import Material.Color as Color
import Material.Icon as Icon
import Material.Elevation as Elevation
import Material.Grid as Grid exposing (grid, cell, size, Device(..))
import Material.Layout as Layout
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Helpers exposing (pure, effect)
import Material.HelpersX exposing (liftCallback)

import Customer exposing (Customer)
import Booking  exposing (Booking, Individual, Room)

import Defaults

import Array exposing (Array)
import Helpers.Array as ArrayX

import Cards.Note as NoteCard
import Cards.Customer as CustomerCard
import Cards.Selection
import Cards.Rooms
import Cards.Individuals
import Cards.Booking

import Date.Format as DateF

import Database as Db exposing (Msg(..))
import Http

main =
  Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }


-- MODEL

type DbState
    = Syncing
    | Synced
    | Error Http.Error

type alias Mdl = Material.Model

type alias Model =
  { customer         : Customer
  , bookings         : Array Booking
  , filter           : String
  , focusedBooking   : Int
  , customerCard     : CustomerCard.Model
  , customerNoteCard : NoteCard.Model
  , individualsCard  : Cards.Individuals.Model
  , roomsCard        : Cards.Rooms.Model
  , bookingNoteCard  : NoteCard.Model
  , bookingCard      : Cards.Booking.Model
  , dbState          : DbState
  , dirty            : Bool
  , mdl              : Mdl
  }

empty : Model
empty =
    { customer = Customer.empty
    , bookings = Array.fromList [Booking.empty]
    , filter = ""
    , customerCard = CustomerCard.init Customer.empty
    , customerNoteCard = NoteCard.init ""
    , bookingNoteCard  = NoteCard.init ""
    , individualsCard  = Cards.Individuals.init []
    , roomsCard        = Cards.Rooms.init []
    , bookingCard      = Cards.Booking.init Booking.empty
    , focusedBooking   = -1
    , dbState = Synced
    , dirty = False
    , mdl = Material.model
    }

init : (Model, Cmd Msg)
init =
  ( empty
  , Db.getLatestCustomer Database "" )

dirty : Model -> Model
dirty m = { m | dirty = True }


-- UPDATE

type Msg
    = New
    | Save
    | Abort
    | Previous
    | Next
    | Last
    | FilterChanged String
    | SelectBooking Int

    | Database (Db.Msg)

    | Ignore

    -- Incoming changes from forms
    | UpdatedCustomer    (Customer -> Customer)
    | UpdatedCustomerNote String
    | UpdatedBookingNote  String
    | UpdatedIndividuals (List Individual)
    | UpdatedRooms       (List Room)
    | UpdatedBooking     (Booking -> Booking)

    -- Pass through
    | CustomerCardMsg     (CustomerCard.Msg Msg)
    | CustomerNoteCardMsg (NoteCard.Msg Msg)
    | BookingNoteCardMsg  (NoteCard.Msg Msg)
    | IndividualsCardMsg  (Cards.Individuals.Msg Msg)
    | RoomsCardMsg        (Cards.Rooms.Msg Msg)
    | BookingCardMsg      (Cards.Booking.Msg Msg)

    -- Material Boilerplate
    | Mdl (Material.Msg Msg)


setCustomer : Customer -> Model -> Model
setCustomer c model =
    let b  = Array.fromList c.bookings
        c_ = { c | bookings = [] }
    in
        { model
        | customer = c_
        , dbState = Synced
        , customerCard = CustomerCard.init c
        , customerNoteCard = NoteCard.init c.note
        , bookings = b
        }
        |> selectBooking 0

selectBooking : Int -> Model -> Model
selectBooking i model =
    case Array.get i model.bookings of
        Nothing -> model
        Just booking ->
            { model
            | focusedBooking  = i
            , individualsCard = Cards.Individuals.init booking.individuals
            , roomsCard       = Cards.Rooms.init       booking.rooms
            , bookingNoteCard = NoteCard.init booking.note
            , bookingCard     = Cards.Booking.init booking
            }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    New ->
        { empty | filter = "" } |> dirty |> pure

    Save ->
        let c_ = model.customer
            c = { c_ | bookings = model.bookings |> Array.toList }
            save =
                Db.saveCustomer Database c
                |> effect
        in
            if not model.dirty then pure model else
              case model.dbState of
                Error _ -> { model | dbState = Syncing } |> save
                Synced  -> { model | dbState = Syncing } |> save
                Syncing -> model |> pure

    Abort ->
      -- TODO: implement abort
      init

    Previous ->
        model |> case model.customer.customer_id of
          Nothing -> pure
          Just i  -> effect <|
              Db.getPrevCustomerById Database model.filter i

    Next ->
        model |> case model.customer.customer_id of
          Nothing -> pure
          Just i  -> effect <|
              Db.getNextCustomerById Database model.filter i

    Last ->
        model |> effect (Db.getLatestCustomer Database model.filter)

    FilterChanged str ->
        { model | filter = str } |>
        effect (Db.getLatestCustomer Database str)

    Database msg_ ->
        case msg_ of
            DbReceived c -> setCustomer c model |> pure
            DbError e -> { model | dbState = Error e } |> pure
            DbSuccess -> { model | dbState = Synced }  |> pure

    SelectBooking i ->
        selectBooking i model |> pure

    Ignore ->
        -- TODO: Where is this needed?
        pure model

    UpdatedCustomer mod ->
        { model | customer = mod model.customer }
        |> dirty
        |> pure

    UpdatedCustomerNote str ->
        let mod c = { c | note = str }
        in
            { model | customer = mod model.customer }
            |> dirty
            |> pure

    UpdatedBookingNote str ->
        let mod b = { b | note = str }
            bookings_ =
                ArrayX.modify model.focusedBooking mod model.bookings
        in
            { model | bookings = bookings_ }
            |> dirty
            |> pure

    UpdatedIndividuals lst ->
        let mod b =
                { b | individuals = lst }
            bookings_ =
                ArrayX.modify model.focusedBooking mod model.bookings
        in
            { model | bookings = bookings_ }
            |> dirty
            |> pure

    UpdatedRooms lst ->
        let mod b =
                { b | rooms = lst }
            bookings_ =
                ArrayX.modify model.focusedBooking mod model.bookings
        in
            { model | bookings = bookings_ }
            |> dirty
            |> pure

    UpdatedBooking mod ->
        { model
        | bookings = ArrayX.modify model.focusedBooking mod model.bookings
        }
        |> dirty |> pure


    CustomerCardMsg msg_ ->
        liftCallback
            { delete = Ignore -- TODO: implement customer deletion
            , updated = UpdatedCustomer
            , mdl = Mdl
            }             .customerCard
            (\m x -> { m | customerCard = x })
            CustomerCard.update
            msg_ model

    BookingCardMsg msg_ ->
        liftCallback
            { delete = Ignore -- TODO: implement booking deletion
            , updated = UpdatedBooking
            , mdl = Mdl
            }             .bookingCard
            (\m x -> { m | bookingCard = x })
            Cards.Booking.update
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

    RoomsCardMsg       msg_ ->
        liftCallback
            { mdl = Mdl
            , updated = UpdatedRooms
            }             .roomsCard
            (\m x -> { m | roomsCard = x })
            Cards.Rooms.update
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


-- VIEW

view : Model -> Html Msg
view model =
    Layout.render Mdl model.mdl [ Layout.fixedHeader ]
        { header = [ controls model ]
        , drawer = []
        , tabs = ( [], [] )
        , main = [ body model ]
        }


customerNoteCfg : NoteCard.Cfg Msg
customerNoteCfg =
    { lift  = CustomerNoteCardMsg
    , index = [800]
    , title = "Anmerkungen"
    }

bookingNoteCfg : NoteCard.Cfg Msg
bookingNoteCfg =
    { lift  = BookingNoteCardMsg
    , index = [801]
    , title = "Notiz zur Buchung"
    }

customerCfg : CustomerCard.Cfg Msg
customerCfg =
    { lift  = CustomerCardMsg
    , index = [802]
    }

individualsCfg : Cards.Individuals.Cfg Msg
individualsCfg =
    { lift  = IndividualsCardMsg
    , index = [803]
    , title = Nothing
    }

roomsCfg : Cards.Rooms.Cfg Msg
roomsCfg =
    { lift  = RoomsCardMsg
    , index = [804]
    , title = Nothing
    }

bookingCfg : Cards.Booking.Cfg Msg
bookingCfg =
    { lift  = BookingCardMsg
    , index = [805]
    }

-- TODO: This seems to have deprecated Card Api
bookingSelectionCfg : Cards.Selection.Cfg Msg Booking.Summary
bookingSelectionCfg =
    let date  d  = Maybe.withDefault "" (Maybe.map (DateF.format "%d.%m.%y") d)
        range el = text (date el.from ++ " bis " ++ date el.to)
        int   i  = text (toString i)
        beds  el = int el.n_beds
        rooms el = int el.n_rooms
        fields =
            [ ( Icon.i "date_range" , range )
            , ( Icon.i "hotel"      , beds  )
            , ( Icon.i "vpn_key"    , rooms )
            ]
    in
    { mdl = Mdl
    , index = [806]
    , render = Cards.Selection.table fields
    , add = Ignore -- TODO: Implement Booking creation
    , select = SelectBooking
    , title = Nothing
    }

body : Model -> Html Msg
body model =
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

        selection = Cards.Selection.view
            bookingSelectionCfg
            model.mdl
            (Array.map Booking.summary model.bookings)
            model.focusedBooking

        -- booking meta information
        booking = Cards.Booking.view
            bookingCfg
            model.mdl
            model.bookingCard

        -- list of individuals, editable

        individuals = Cards.Individuals.view
            individualsCfg
            model.mdl
            model.individualsCard

        -- list of rooms, editable

        rooms = Cards.Rooms.view
            roomsCfg
            model.mdl
            model.roomsCard

        -- booking note

        bookingNote = NoteCard.view
            bookingNoteCfg
            model.mdl
            model.bookingNoteCard
    in
        grid
            [ Grid.noSpacing
            ]
            [ cell [ size All 4 ] [ customer, selection, customerNote ]
            , cell [ size All 4 ] [ booking, rooms ]
            , cell [ size All 4 ] [ individuals, bookingNote ]
            ]


controls : Model -> Html Msg
controls model =
    let button cond = Defaults.button_
            [ Button.disabled |> Options.when (not cond)
            , Button.raised
            ] Mdl model.mdl

        dirty = model.dirty

        ndirty = not dirty

        filter =
            Textfield.render Mdl [0] model.mdl
                [ Textfield.label "Suche"
                , Textfield.text_
                , Textfield.value model.filter
                , Textfield.disabled |> Options.when dirty
                , Options.onInput (FilterChanged)
                ] []

        filterIcon = Icon.view "search"
            [ Options.css "margin-right" "5px" ]

    in
        Layout.row
            [ Color.background Color.accent
            , Color.text Color.primary
            ]
            [ filterIcon
            , filter
            , Layout.spacer
            , button ndirty [1] "chevron_left"  Previous
            , button ndirty [2] "chevron_right" Next
            , button ndirty [3] "last_page"     Last
            , Layout.spacer
            , button ndirty [4] "library_add" New
            , button dirty  [5] "save"        Save
            , button dirty  [6] "cancel"      Abort
            ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


