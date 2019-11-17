# GHM

Guesthouse management using [incr_dom](gh:incr_dom) (work in progress).

[gh:incr_dom]: https://github.com/janestreet/incr_dom

## jq

Migration from old production api (single table) to new schema:
```bash
jq 'map({ id:.customer_id, data: (.data | del(.bookings)), bookings: .data.bookings }) | { customers: . }' < data/combit.json > data/combit.migrated.json
```

## ToDo

* save/fetch bookings separately from customers
* Json Export for jMeldeschein
