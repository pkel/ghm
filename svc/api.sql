set search_path=api;

create table customers (
    id bigint generated always as identity primary key,
    created timestamp not null default now (),
    modified timestamp not null default now (),
    data jsonb not null,
    keyword text not null);

create table bookings (
    id bigint generated always as identity primary key,
    customer bigint references customers,
    created timestamp not null default now (),
    modified timestamp not null default now (),
    data jsonb not null,
    arrival date not null,
    departure date not null);

create table invoice_numbers (
    number text primary key,
    booking bigint references bookings);

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
