#!/bin/bash

root=./secrets

value () {
  mkdir -p "$(dirname "$root/$1")"
  cat /dev/stdin > "$root/$1"
}

password () {
  mkdir -p "$(dirname "$root/$1")"
  tr -dc _A-Z-a-z-0-9 < /dev/urandom | head -c48 > "$root/$1"
  echo >> "$root/$1"
}

value    db-cfg/db-name <<< "ghm"

value    db-user-root/username <<< "administrator"
password db-user-root/password

value    db-user-auth/username <<< "authenticator"
password db-user-auth/password

value    db-user-api/username <<< "postgrest"
password db-user-api/password

password jwt/secret

value    api-user-devel/username <<< "$(id -un)"
password api-user-devel/password
