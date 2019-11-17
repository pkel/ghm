#!/bin/bash
set -e

secret () {
  cat "/secrets/$1"
}

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" << EOF
-- php login form uses this role
create role $(secret db-user-auth/username) noinherit login
  password '$(secret db-user-auth/password)';

-- postgrest connects as this user
create role $(secret db-user-api/username) noinherit login
  password '$(secret db-user-api/password)';

-- authenticated ghm user via api (after jwt)
create role ghm_user nologin noinherit;
grant ghm_user to $(secret db-user-api/username);

-- unauthenticated user via api (after jwt)
create role anonymous nologin noinherit;
grant anonymous to $(secret db-user-api/username);

-- auth role may access crypto and user table
grant usage on schema auth, jwt, crypto to $(secret db-user-auth/username);
grant select on auth.users to $(secret db-user-auth/username);

-- ghm api users can work on api.*
grant usage on schema api to ghm_user;
grant select, insert, update, delete
  on all tables in schema api
  to ghm_user;

-- ghm api user for development
insert into auth.users(id, pass, role)
  values('$(secret api-user-devel/username)',
         '$(secret api-user-devel/password)',
         'ghm_user');
EOF
