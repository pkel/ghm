ghm - Guesthouse Manager
========================

Booking management built in elm and postgres(t).

Todo
----

  * Trim data on insert/update
  * Prevent inserts with higher id's than serial currently is
  * Avoid empty bookings on import
  * DB not available message

Next steps
----------

  * Insert new bookings for existing customers
  * Customer generation
  * saving

Gui Tuning
----------

  * Use Card.head/subhead/expand
  * Markdown editing in monospace font
  * Calculate number of nights for booking selection
  * Calculate number of persons for booking selection

External Dependencies
---------------------

```
dnf install docker docker-compose npm postgresql
systemctl start docker
```

Usage
-----

```
cd postgrest ; make clean init combit
make run
```
```
cd ../elm ; make dependencies
make watch
```
