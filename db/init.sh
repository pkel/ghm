#!/bin/bash

set -e

source ./env

psql="sudo docker-compose exec -e PGPASSWORD=$pg_pwd -T db psql -U $pg_user $pg_db"

echo clean
$psql < clean.sql
echo jwt
$psql < jwt.sql
echo auth
$psql < auth.sql
echo data
$psql < data.sql
echo dummy
$psql < dummy.sql
