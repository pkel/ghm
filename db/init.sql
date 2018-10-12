drop table if exists customers cascade;

create table customers (
    customer_id  bigint generated always as identity primary key,
    data         jsonb not null );
