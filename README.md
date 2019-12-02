# GHM

Guesthouse management using [incr_dom](gh:incr_dom) (work in progress).

[gh:incr_dom]: https://github.com/janestreet/incr_dom

## jq

Migration from old production api (single table) to new schema:
```bash
jq 'map({ id:.customer_id, data: (.data | del(.bookings)), bookings: .data.bookings }) | { customers: . }' < data/combit.json > data/combit.migrated.json
```

Or only a subset for speedy import:
```bash
jq 'map(select(.customer_id >= 12000 and .customer_id <= 12100) | { id:.customer_id, data: (.data | del(.bookings)), bookings: .data.bookings }) | { customers: . }' < data/combit.json > data/combit.migrated.json
```

## ToDo

* PHP puts cleartext password to error log / client on database
  connection failure. Either wrap usage of password into try catch
  or avoid PHP altogether.
* Communicate with letter js via local storage. CORS should then avoid XSS.
* Overview_view
  - Load today's bookings and view links: departing | staying | arriving
* Booking_view
  - Initialize new booking from current or last booking
  - Json Export for jMeldeschein
  - Highlight current location in menu
* Lock new booking and similar unsaveable views for unsaved new customer.
