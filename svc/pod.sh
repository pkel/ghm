#!/bin/bash

secret () {
  cat "./secrets/$1"
}

set -xe

podman build -f Dockerfile-for-php -t ghm_php
podman build -f Dockerfile-for-nginx -t ghm_nginx

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
  -e POSTGRES_DB_FILE=/secrets/db-cfg/db-name \
  -e POSTGRES_USER_FILE=/secrets/db-user-root/username \
  -e POSTGRES_PASSWORD_FILE=/secrets/db-user-root/password \
  -v ./secrets:/secrets:z \
  -v ./initdb.d:/docker-entrypoint-initdb.d:z \
  postgres:12

rm_container ghm_api
podman run -d --pod ghm --name ghm_api \
  -e PGRST_SERVER_HOST='*4' \
  -e PGRST_SERVER_PORT="3000" \
  -e PGRST_DB_ANON_ROLE="anonymous" \
  -e PGRST_DB_SCHEMA="api" \
  -e PGRST_DB_URI="postgres://$(secret db-user-api/username):$(secret db-user-api/password)@localhost:5432/$(secret db-cfg/db-name)" \
  -e PGRST_JWT_SECRET="$(secret jwt/secret)" \
  -e PGRST_SECRET_IS_BASE64="false" \
  postgrest/postgrest

rm_container ghm_php
rm_container ghm_nginx

if [ -z "$MOUNT_WEBROOT" ]
then
  podman run -d --pod ghm --name ghm_php \
    -e DB_HOST=localhost \
    -e DB_PORT=5432 \
    -e DB_NAME="$(secret db-cfg/db-name)" \
    -e DB_USER="$(secret db-user-auth/username)" \
    -e DB_PASS="$(secret db-user-auth/password)" \
    -e JWT_SECRET="$(secret jwt/secret)" \
    ghm_php
  podman run -d --pod ghm --name ghm_nginx \
    ghm_nginx
else
  podman run -d --pod ghm --name ghm_php \
    -e DB_HOST=localhost \
    -e DB_PORT=5432 \
    -e DB_NAME="$(secret db-cfg/db-name)" \
    -e DB_USER="$(secret db-user-auth/username)" \
    -e DB_PASS="$(secret db-user-auth/password)" \
    -e JWT_SECRET="$(secret jwt/secret)" \
    -v ./webroot:/var/www/html:z \
    ghm_php
  podman run -d --pod ghm --name ghm_nginx \
    -v ./webroot:/var/www/html:z \
    ghm_nginx
fi
