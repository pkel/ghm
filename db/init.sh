#!/bin/bash

set -e

source .env

psql="sudo docker-compose exec -e PGPASSWORD=$pg_pwd -T db psql -U $pg_user $pg_db"

sql () {
  echo "--- executing $1.sql"
  $psql < "$1.sql"
}

sql clean

authenticator_pwd="$(tr -dc _A-Z-a-z-0-9 < /dev/urandom | head -c48)"

$psql << EOF
create role authenticator noinherit login password '$authenticator_pwd';
EOF

jwt_secret="$(tr -dc _A-Z-a-z-0-9 < /dev/urandom | head -c48)"

cat << EOF > postgrest.conf
db-uri = "postgres://authenticator:$authenticator_pwd@db:5432/$pg_db"
db-schema = "api"
db-anon-role = "anonymous"
db-extra-search-path = "jwt"

server-host = "*4"
server-port = 3000

jwt-secret = "$jwt_secret"
app.settings.jwt_secret = "$jwt_secret"
secret-is-base64 = false

server-proxy-url = "${base_uri}/api/"
EOF

sql jwt
sql auth
sql api
sql dummy
