/* customers */

drop table if exists customers cascade;

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


/* authentication */

drop role if exists anonymous;
drop role if exists ghm_user;

-- users with minimal rights
create role anonymous with noinherit nologin;
create role ghm_user with noinherit nologin;

grant all on customers to ghm_user;

/*
drop schema if exists auth;
create schema auth;

create table auth.users (
  name text not null,
  pass text not null);
*/
