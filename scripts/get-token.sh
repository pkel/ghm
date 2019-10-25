#!/bin/bash

set -e

source svc/.env
curl -d "action=token&username=$app_user&password=$app_pass" $base_uri/
