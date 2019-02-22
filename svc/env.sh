#!/bin/bash

db_auth_pass="$(tr -dc _A-Z-a-z-0-9 < /dev/urandom | head -c48)"
db_root_pass="$(tr -dc _A-Z-a-z-0-9 < /dev/urandom | head -c48)"
jwt_secret="$(tr -dc _A-Z-a-z-0-9 < /dev/urandom | head -c48)"
app_pass="$(tr -dc _A-Z-a-z-0-9 < /dev/urandom | head -c48)"

cat << EOF
base_uri=http://localhost:2015
db_name=ghm
db_root_user=administrator
db_root_pass=$db_root_pass
db_auth_user=authenticator
db_auth_pass=$db_auth_pass
jwt_secret=$jwt_secret
app_user=$(id -un)
app_pass=$app_pass
EOF
