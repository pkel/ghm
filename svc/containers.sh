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

net="$prefix-net"
pod="$prefix-pod"

link () {
  if [[ -v ROOT ]]
  then
    echo "$prefix-container-$1"
  else
    echo "localhost"
  fi
}

rm_container () {
  $docker rm "$(container "$1")" -f -v || true
}
rm_container db
rm_container api
rm_container php
rm_container nginx

if [[ -v ROOT ]]
then
  $docker network rm "$net" || true
  $docker network create "$net"
  net_options=(--network "$net")
else
  if $docker pod exists "$pod"
  then
    $docker pod rm -f "$pod"
  fi
  $docker pod create --name "$pod" -p 8080:80
  net_options=(--pod "$pod")
fi

$docker build -f Dockerfile-for-php -t "$(image php)" .
$docker build -f Dockerfile-for-nginx -t "$(image nginx)" .

$docker run -d --name "$(container db)" \
  "${net_options[@]}" \
  -e POSTGRES_DB_FILE=/secrets/db-cfg/db-name \
  -e POSTGRES_USER_FILE=/secrets/db-user-root/username \
  -e POSTGRES_PASSWORD_FILE=/secrets/db-user-root/password \
  -v "$(pwd)/secrets:/secrets:z" \
  -v "$(pwd)/initdb.d:/docker-entrypoint-initdb.d:z" \
  postgres:12

$docker run -d --name "$(container api)" \
  "${net_options[@]}" \
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
  mount_options=(-v "$(pwd)/webroot:/var/www/html:z")
else
  mount_options=()
fi

$docker run -d --name "$(container php)" \
  "${net_options[@]}" \
  -e DB_HOST="$(link db)" \
  -e DB_PORT=5432 \
  -e DB_NAME="$(secret db-cfg/db-name)" \
  -e DB_USER="$(secret db-user-auth/username)" \
  -e DB_PASS="$(secret db-user-auth/password)" \
  -e JWT_SECRET="$(secret jwt/secret)" \
  "${mount_options[@]}" \
  "$(image php)"

if [[ -v ROOT ]]
then
  port_options=(-p 80:80)
else
  port_options=()
fi

$docker run -d --name "$(container nginx)" \
  "${net_options[@]}" \
  "${port_options[@]}" \
  -e PHP_HOST="$(link php)" \
  -e API_HOST="$(link api)" \
  "${mount_options[@]}" \
  "$(image nginx)"
