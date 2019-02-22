#!/bin/bash

source svc/.env
curl -c .cookies -d "action=login&username=$app_user&password=$app_pass" $base_uri/
curl -b .cookies $base_uri/token.php
rm .cookies
