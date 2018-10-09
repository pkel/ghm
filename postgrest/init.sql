drop table if exists customers cascade;

create table customers (
    customer_id     bigserial primary key,
    data            jsonb     not null );
