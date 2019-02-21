drop schema if exists api, auth, jwt, crypto cascade;

drop extension if exists pgcrypto;

create schema api;
create schema auth;
create schema crypto;
create schema jwt;
