/* customers */

set search_path=api;

create table customers (
    customer_id bigint generated always as identity primary key,
    created timestamp not null default now (),
    modified timestamp not null default now (),
    data jsonb not null);

-- automatic ids
create or replace function set_modified_now ()
returns trigger as $$
begin
   new.modified = now();
   return new;
end;
$$ language 'plpgsql';

create trigger customers_set_modified_now before update on customers
for each row execute procedure set_modified_now ();

-- keyword search
create or replace function keyword(customers) returns text as $$
  select $1.data->>'keyword';
$$ language sql;

grant all on customers to ghm_user;
