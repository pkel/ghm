#!/bin/bash

set -e

source .env

psql="sudo docker-compose exec -e PGPASSWORD=$db_root_pass -T db psql -U $db_root_user $db_name"

sql () {
  echo "--- executing $1.sql"
  $psql < "$1.sql"
  echo "--- done executing $1.sql"
}

sql clean

$psql << EOF
drop role if exists ghm_user;
drop role if exists postgrest;
drop role if exists anonymous;
drop role if exists $db_auth_user;
EOF

db_postgrest_pass="$(tr -dc _A-Z-a-z-0-9 < /dev/urandom | head -c48)"

$psql << EOF
-- php login
create role $db_auth_user noinherit login password '$db_auth_pass';

-- postgrest before jwt interpretation
create role postgrest noinherit login password '$db_postgrest_pass';

-- authenticated ghm user via api
create role ghm_user nologin noinherit;
grant ghm_user to postgrest;

-- unauthenticated user via api
create role anonymous nologin noinherit;
grant anonymous to postgrest;
EOF

sql jwt # jwt and crypto extensions
sql auth # user table

$psql << EOF
grant usage on schema auth, jwt, crypto to $db_auth_user;
grant select on auth.users to $db_auth_user;
EOF

sql api # actual schema
$psql << EOF
grant usage on schema api to ghm_user;
grant all on api.customers to ghm_user;
EOF

# generate ghm user for development
$psql << EOF
insert into auth.users(id, pass, role) values('$app_user', '$app_pass', 'ghm_user');
EOF

# generate postgrest config based on generated secrets
cat << EOF > postgrest.conf
db-uri = "postgres://postgrest:$db_postgrest_pass@db:5432/$db_name"
db-schema = "api"
db-anon-role = "anonymous"

server-host = "*4"
server-port = 3000

jwt-secret = "$jwt_secret"
secret-is-base64 = false

server-proxy-url = "${base_uri}/api/"
EOF
