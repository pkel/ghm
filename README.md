# GHM

Guesthouse management using [incr_dom](gh:incr_dom) (work in progress).

[gh:incr_dom]: https://github.com/janestreet/incr_dom

## jq

Migration from old production api (single table) to new schema:
```bash
jq 'map({ id:.customer_id} + . + {bookings:[{customer:.customer_id, id:-1, created: .created, modified: .modified, data: .data.bookings[]}]} | del(.customer_id, .data.bookings)) | {customers: [.[] | del(.bookings)], bookings: [.[].bookings[]]}' < data/some.json > data/some.migrated.json
```

## ToDo

* Booking_view
  - Initialize new booking from current or last booking
  - Json Export for jMeldeschein
  - Invoice positions add/delete/reorder
* Visually lock new booking and similar unsaveable views for unsaved new
  customer. Currently saving is silently deferred.
* Avoid janestreet core_kernel dependency. The only thing we use
  currently is Date.t
* Refactor nav fields of component models into incremental argument. Use
  this for view. We achieve nav being read-only input for component.
  Mutable nav makes no sense.
* BUG: URL `customer/<cid>/booking/<bid>` opens booking bid under
  customer cid. This moves booking bid to customer cid on next save.
* SEC: Communicate with letter js via local storage. CORS should then avoid XSS.
* SEC: PHP puts cleartext password to error log / client on database
  connection failure. Either wrap usage of password into try catch
  or avoid PHP altogether.
