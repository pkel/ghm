#!/bin/bash

source ./env

cat << EOF
db-uri = "postgres://authenticator:todo_randomize@db:5432/${pg_db}"
db-schema = "api"
db-anon-role = "anonymous"
db-extra-search-path = "jwt"

server-host = "*4"
server-port = 3000

jwt-secret = "nqMWgbS7vshXFei4eETpuffWRq4PvhTu"
app.settings.jwt_secret = "nqMWgbS7vshXFei4eETpuffWRq4PvhTu"
secret-is-base64 = false

server-proxy-url = "${base_uri}/api/"
EOF
