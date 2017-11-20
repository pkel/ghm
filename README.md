ghm - Guesthouse Manager
========================

Booking management built in [elm][elm] and [postgrest][postgrest]. Work
in progress. The long term goal is to replace a proprietary windows-only
software with a browser based solution. Thus the repo contains a
distracting migration script referred to as *combit*.

Structure
---------

You see two folders [postgrest](postgrest) and [elm](elm). The former
contains the docker/postgres/[postgrest][postgrest] backend, the latter
is a compile-to-javascript frontend using [elm][elm]. Both folders
contain Makefiles that document scarcely how stuff is glued together.

[postgrest]: https://github.com/begriffs/postgrest
[elm]: http://elm-lang.org/

Warning
-------

This is not ready for production. The lack of access control contradicts
use outside of localhost. Furthermore, the interface is available in
German only.

External Dependencies
---------------------

Install some dependencies. May be out of date.

```
dnf install docker docker-compose npm postgresql
systemctl start docker
```

Usage
-----

Init backend and optionally migrate data from old software. Start the
service, attach to logging (Ctrl+C to exit) and stop it after work.

```
cd /postgrest
make init [import]
make up
make log
make down
```

Then build the frontend and browse to `file:///elm/index.html`
alternatively you can use `make serve` and `localhost:5000`.

```
cd /elm
make dependencies
make watch
```

Next steps
----------

General:
  * Basic access control

Backend:
  * trim data on insert/update
  * prevent inserts with higher id's than serial currently is
  * avoid empty bookings on import

Frontend:
  * DB not available message
  * error handling in general
  * test: json encode = json decode ^ -1

Style:
  * use Card.head/subhead/expand
  * markdown editing in monospace font
