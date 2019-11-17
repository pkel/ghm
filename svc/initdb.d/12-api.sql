create schema api;
set search_path=api;

create table customers (
    id bigint generated always as identity primary key check (id >= 0),
    created timestamp not null default now (),
    modified timestamp not null default now (),
    data jsonb not null,
    /* extract keyword from data for filtering */
    keyword text generated always as (data->>'keyword') stored not null);

create table bookings (
    id bigint generated always as identity primary key check (id >= 0),
    customer bigint not null references customers on delete cascade,
    created timestamp not null default now (),
    modified timestamp not null default now (),
    data jsonb not null,
    /* extract dates from data for filtering (* TODO *) */
    arrival text generated always as (data->'period'->>0) stored not null,
    departure text generated always as (data->'period'->>1) stored not null);

create table invoice_numbers (
    number text primary key,
    booking bigint not null references bookings on delete restrict);

/* automatic modification timestamps */
create or replace function set_modified_now ()
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
