create schema db;

set search_path to db;

create table customers (
    customer_id bigserial primary key,

    title            text      not null default '',
    title_letter     text      not null default '',

    given            text      not null default '',
    second           text      not null default '',
    family           text      not null default '',

    company          text      not null default '',
    company_address  text      not null default '',

    street           text      not null default '',
    street_number    text      not null default '',
    city             text      not null default '',
    postal_code      text      not null default '',
    country          text      not null default '',
    country_code     text      not null default '',

    phone            text      not null default '',
    phone2           text      not null default '',
    mobile           text      not null default '',
    fax              text      not null default '',
    fax2             text      not null default '',
    mail             text      not null default '',
    mail2            text      not null default '',
    web              text      not null default '',

    keyword          text      not null default '',
    note             text      not null default ''
);

/* We want to log changes to this table */

create table customers_log (like customers);

alter table customers_log
  add column customers_log_id bigserial primary key,
  add column log_date timestamp default current_timestamp;

/* TODO: catch non-changes */
create rule log_customers as on update to customers
  do insert into customers_log values (old.*);


create table rooms (
    room_id           bigserial        primary key,
    name              text not null,
    available bool not null default true,
    beds              int not null check (beds > 0)
);

create table booking_states (
    booking_state_id  bigserial primary key,
    name text                   not null,
    consider bool               not null
);

create table bookings (
    booking_id        bigserial primary key,
    customer_id       bigint    references customers on delete cascade on update cascade,
    state             int       references booking_states not null default '0',
    deposit_asked     numeric   check (deposit_asked >= 0),
    deposit_got       numeric   check (deposit_got >= 0),
    no_tax            bool      not null default false,
    note              text      not null default ''
);

create table booking_rooms (
    booking_room_id   bigserial primary key,
    booking_id        bigint    references bookings on delete cascade on update cascade not null,
    room_id           bigint    references rooms,

    beds              int       check (beds > 0),
    price_per_bed     numeric   not null default 0,
    factor            numeric   not null default 0 check (factor >= 0),

    description       text not null default '',

    breakfast         bool not null default true,

    note              text not null default '',

    from_date         date,
    to_date           date
);

create table booking_individuals (
    booking_individual_id
                      bigserial        primary key,
    booking_id        bigint           references bookings on delete cascade on update cascade not null,

    given             text not null default '',
    second            text not null default '',
    family            text not null default '',
    year_of_birth     int,
    month_of_birth    int,
    day_of_birth      int
);

