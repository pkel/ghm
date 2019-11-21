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

* PHP puts cleatext password to error log / client on database
  connection failure. Either wrap usage of password into try catch
  or avoid PHP altogether
* save/fetch bookings separately from customers
* Json Export for jMeldeschein
