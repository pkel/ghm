#!/bin/bash
set -xe

podman build -f Dockerfile-for-php -t ghm_php
podman build -f Dockerfile-for-nginx -t ghm_nginx

source .env

if podman pod exists ghm
then
  podman pod rm ghm -f
fi

podman pod create --name ghm -p 8080:80

rm_container () {
  if podman container exists "$1"
  then
    podman rm "$1"
  fi
}

rm_container ghm_db
podman run -d --pod ghm --name ghm_db \
  -e POSTGRES_DB="${db_name}" \
  -e POSTGRES_USER="${db_root_user}" \
  -e POSTGRES_PASSWORD="${db_root_pass}" \
  postgres:12

while ! ./init.sh ; do sleep 1 ; done

rm_container ghm_api
podman run -d --pod ghm --name ghm_api \
  -e PGRST_SERVER_HOST='*4' \
  -e PGRST_SERVER_PORT="3000" \
  -e PGRST_DB_ANON_ROLE="anonymous" \
  -e PGRST_DB_SCHEMA="api" \
  -e PGRST_DB_URI="postgres://$db_postgrest_user:$db_postgrest_pass@localhost:5432/$db_name" \
  -e PGRST_JWT_SECRET="$jwt_secret" \
  -e PGRST_SECRET_IS_BASE64="false" \
  postgrest/postgrest

rm_container ghm_php
podman run -d --pod ghm --name ghm_php \
  -e DB_HOST=localhost \
  -e DB_PORT=5432 \
  -e DB_NAME="${db_name}" \
  -e DB_USER="${db_auth_user}" \
  -e DB_PASS="${db_auth_pass}" \
  -e JWT_SECRET="${jwt_secret}" \
  ghm_php

rm_container ghm_nginx
podman run -d --pod ghm --name ghm_nginx ghm_nginx
