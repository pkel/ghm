create schema api;
set search_path=api;

create table customers (
    id bigint generated always as identity primary key check (id >= 0),
    created timestamp not null default now (),
    modified timestamp not null default now (),
    data jsonb not null,
    /* extract keyword from data for filtering */
    keyword text generated always as (data->>'keyword') stored not null);

create function arrival_of_booking(data jsonb) returns date as $$
begin
  return least(date(data->'period'->>0), date(data->'period'->>1));
end;
$$ language 'plpgsql' immutable;

create function departure_of_booking(data jsonb) returns date as $$
begin
  return greatest(date(data->'period'->>0), date(data->'period'->>1));
end;
$$ language 'plpgsql' immutable;

create table bookings (
    id bigint generated always as identity primary key check (id >= 0),
    customer bigint not null references customers on delete cascade,
    created timestamp not null default now (),
    modified timestamp not null default now (),
    data jsonb not null,
    /* extract dates from data for filtering */
    arrival date generated always as (arrival_of_booking(data)) stored not null,
    departure date generated always as (departure_of_booking(data)) stored not null);

create table invoice_numbers (
    number text primary key,
    booking bigint not null references bookings on delete restrict);

/* automatic modification timestamps */
create function set_modified_now ()
returns trigger as $$
begin
   new.modified = now();
   return new;
end;
$$ language 'plpgsql';

create trigger customers_set_modified_now before update on customers
for each row execute procedure set_modified_now ();

create trigger bookings_set_modified_now before update on bookings
for each row execute procedure set_modified_now ();
