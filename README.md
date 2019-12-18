# GHM

Guesthouse management using [incr_dom](gh:incr_dom) (work in progress).

[gh:incr_dom]: https://github.com/janestreet/incr_dom

## jq

Migration from old production api (single table) to new schema:
```bash
jq 'map({ id:.customer_id} + . + {bookings:[{created: .created, modified: .modified} + .data.bookings[]]} | del(.customer_id, .data.bookings))' < data/some.json > data/some.migrated.json
```

## ToDo

* PHP puts cleartext password to error log / client on database
  connection failure. Either wrap usage of password into try catch
  or avoid PHP altogether.
* Communicate with letter js via local storage. CORS should then avoid XSS.
* Booking_view
  - Initialize new booking from current or last booking
  - Json Export for jMeldeschein
  - Highlight current location in menu
* Lock new booking and similar unsaveable views for unsaved new customer.
* It seems like `customer/<cid>/booking/<bid>` opens booking bid under
  customer cid. This might move booking bid to cid on next save.
