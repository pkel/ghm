# GHM

Guesthouse management using [incr_dom](gh:incr_dom).

[gh:incr_dom]: https://github.com/janestreet/incr_dom

## ToDo

- fetch customers as required / specified by search
- distinguish between table row id / customer id

- revise/drop storage.mli
- switch to atdgen?

- allow creation/deletion of customer/booking/guest/room
- automatic save (on leave / by time)

- overview: icons for room/guest/bed counts
- overview: currently checked-in, upcoming

- indicate saving/loading state
- handle 50x status -> api/server not available
- title handling (variant + dropdown)
- indicate units in form fields (%, €, ...)

- backwards compatible export to clipboard for billing
- letter templates (first clipboard, then mailto / openoffice / print)

- advanced search, potentially powered by database backend
- migration: street number, #beds, multiple rooms
- authentication with backend
- smart form: country code, zip and area code lookup; first guest from
  main data; further guest with pre-filled family name; room description
  and price auto completion
- export to jmeldeschein (or even API consumption)

- in-app billing or interop with libreoffice
- room allocation tool/view
- offline mode
