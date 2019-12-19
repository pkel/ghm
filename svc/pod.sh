#!/bin/bash

set -euxo pipefail

docker=${DOCKER:-podman}
prefix=${PREFIX:-ghm}

secret () {
  cat "./secrets/$1"
}

image () {
  echo "$prefix-image-$1"
}

container () {
  echo "$prefix-container-$1"
}

link () {
  if [[ -v ROOT ]]
  then
    echo "$prefix-container-$1"
  else
    echo "localhost"
  fi
}

if [[ -v ROOT ]]
then
  pod_options=()
else
  if $docker pod exists "$prefix-pod"
  then
    $docker pod rm -f "$prefix-pod"
  fi
  $docker pod create --name "$prefix-pod" -p 8080:80
  pod_options=(--pod "$prefix-pod")
fi

$docker build -f Dockerfile-for-php -t "$(image php)"
$docker build -f Dockerfile-for-nginx -t "$(image nginx)"

rm_container () {
  if $docker container exists "$(container "$1")"
  then
    $docker rm "$(container "$1")" -f
  fi
}

rm_container db
$docker run -d --name "$(container db)" \
  "${pod_options[@]}" \
  -e POSTGRES_DB_FILE=/secrets/db-cfg/db-name \
  -e POSTGRES_USER_FILE=/secrets/db-user-root/username \
  -e POSTGRES_PASSWORD_FILE=/secrets/db-user-root/password \
  -v ./secrets:/secrets:z \
  -v ./initdb.d:/docker-entrypoint-initdb.d:z \
  postgres:12

rm_container api
$docker run -d --name "$(container api)" \
  "${pod_options[@]}" \
  -e PGRST_SERVER_HOST='*4' \
  -e PGRST_SERVER_PORT="3000" \
  -e PGRST_DB_ANON_ROLE="anonymous" \
  -e PGRST_DB_SCHEMA="api" \
  -e PGRST_DB_URI="postgres://$(secret db-user-api/username):$(secret db-user-api/password)@$(link db):5432/$(secret db-cfg/db-name)" \
  -e PGRST_JWT_SECRET="$(secret jwt/secret)" \
  -e PGRST_SECRET_IS_BASE64="false" \
  postgrest/postgrest

if [[ -v MOUNT_WEBROOT ]]
then
  mount_options=(-v ./webroot:/var/www/html:z)
else
  mount_options=()
fi

rm_container php
podman run -d --name "$(container php)" \
  "${pod_options[@]}" \
  -e DB_HOST="$(link db)" \
  -e DB_PORT=5432 \
  -e DB_NAME="$(secret db-cfg/db-name)" \
  -e DB_USER="$(secret db-user-auth/username)" \
  -e DB_PASS="$(secret db-user-auth/password)" \
  -e JWT_SECRET="$(secret jwt/secret)" \
  "${mount_options[@]}" \
  "$(image php)"

rm_container nginx
podman run -d --name "$(container nginx)" \
  "${pod_options[@]}" \
  -e PHP_HOST="$(link php)" \
  -e API_HOST="$(link api)" \
  "${mount_options[@]}" \
  "$(image nginx)"
