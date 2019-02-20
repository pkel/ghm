#!/bin/bash

pg_pwd="$(tr -dc _A-Z-a-z-0-9 < /dev/urandom | head -c48)"

cat << EOF
pg_user=postgrest
pg_pwd="$pg_pwd"
pg_db=postgrest
base_uri=http://localhost:2015
EOF
