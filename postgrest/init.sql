set search_path to public;

create table customers (
    customer_id     bigserial primary key,

    title           text      not null default '',
    title_letter    text      not null default '',

    given           text      not null default '',
    second          text      not null default '',
    family          text      not null default '',

    company         text      not null default '',
    company_address text      not null default '',

    street          text      not null default '',
    street_number   text      not null default '',
    city            text      not null default '',
    postal_code     text      not null default '',
    country         text      not null default '',
    country_code    text      not null default '',

    phone           text      not null default '',
    phone2          text      not null default '',
    mobile          text      not null default '',
    fax             text      not null default '',
    fax2            text      not null default '',
    mail            text      not null default '',
    mail2           text      not null default '',
    web             text      not null default '',

    keyword         text      not null default '',
    note            text      not null default '',

    client          text      not null default ''
);

create table bookings (
    booking_id        bigserial primary key,
    customer_id       bigint    references customers on delete cascade on update cascade,

    state             text      not null default '',
    deposit_asked     numeric   check (deposit_asked >= 0),
    deposit_got       numeric   check (deposit_got >= 0),
    from_date         date,
    to_date           date,
    no_tax            bool      not null default false,
    note              text      not null default '',
    rooms             jsonb     not null default '[]'::jsonb,
    individuals       jsonb     not null default '[]'::jsonb,

    client            text      not null default ''
);

