ghm - Guesthouse Manager
========================

Booking management built in elm and postgres(t).

Todo
----

  * Trim data on insert/update
  * Prevent inserts with higher id's than serial currently is
  * Migrate missing fields from combit to ghm
      - Kundennr, Kategorie: Bemerkung/Sideinfo -> Notiz
      - Plzp, Postfach: Kommentar/2te Adresse -> Notiz
      - Geburtsdaten

Next steps
----------

  * Build elm Type for Booking
  * Parse type from json
  * fetch booking for each customer from db
  * show data in FE
  * Build form to edit booking
  * Insert new bookings for existing customers
