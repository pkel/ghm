drop schema if exists api, auth, jwt, crypto cascade;

drop extension if exists pgcrypto;

drop role if exists ghm_user;
drop role if exists authenticator;
drop role if exists anonymous;

create schema api;
create schema auth;
create schema crypto;
create schema jwt;
