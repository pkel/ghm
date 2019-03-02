#!/bin/bash

set -e

source .env

psql="sudo docker-compose exec -e PGPASSWORD=$db_root_pass -T db psql -U $db_root_user $db_name"

# overwrite ghm user with weak password for development
$psql << EOF
delete from auth.users where id = '$app_user';
insert into auth.users(id, pass, role) values('$app_user', 'devel', 'ghm_user');
EOF

sed -i 's/^app_pass=.*$/app_pass=devel/' .env
