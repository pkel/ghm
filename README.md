# GHM

Guesthouse management using [incr_dom](gh:incr_dom).

[gh:incr_dom]: https://github.com/janestreet/incr_dom

## ToDo

- revise/drop storage.mli
- switch to atdgen?

- automatic save (on leave / by time)
- customer_form model as set of form states
- new booking/guest/room: copy the last one / sensible defaults

- indicate unsaved state
- hide customer form while no data present
- indicate is saving/loading state
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
- overview: currently checked-in, upcoming

- remove font-awesome bits, if not used
