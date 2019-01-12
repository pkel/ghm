drop table if exists customers cascade;

create table customers (
    customer_id bigint generated always as identity primary key,
    created timestamp not null default now (),
    modified timestamp not null default now (),
    data jsonb not null);

/* automatic ids */

create or replace function set_modified_now ()
returns trigger as $$
begin
   new.modified = now();
   return new;
end;
$$ language 'plpgsql';

create trigger customers_set_modified_now before update on customers
for each row execute procedure set_modified_now ();

/* keyword search */

create or replace function keyword(customers) returns text as $$
  select $1.data->>'keyword';
$$ language sql;

/* letter templates */

drop table if exists templates;

create table templates (
  name text not null,
  subject text not null,
  body text not null);

insert into templates(name, subject, body) values('Leer', '', '');
insert into templates(name, subject, body) values('Bestätigung', 'Buchungsbestätigung', 'hiermit bestätigen wir Ihre Buchung');
insert into templates(name, subject, body) values('Schlüssel', 'Herzlich Willkommen', 'hierin finden Sie ihren Schlüssel');
