SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

DOCKER ?= podman
CONTAINERS = ghm-container-db ghm-container-api ghm-container-php ghm-container-nginx

.PHONY: up
up:
	if [[ -v ROOT ]]
	then ${DOCKER} container start $(CONTAINERS) || true
	else ${DOCKER} pod start ghm-pod || true
	fi

.PHONY: down
down:
	if [[ -v ROOT ]]
	then ${DOCKER} container stop $(CONTAINERS) || true
	else ${DOCKER} pod start ghm-pod || true
	fi

.PHONY: containers
containers:
	MOUNT_WEBROOT=1 ./containers.sh

.PHONY: dump.sql
dump.sql:
	touch $@
	chmod 600 $@
	${DOCKER} exec ghm-container-db pg_dump -U $(shell cat secrets/db-user-root/username) $(shell cat secrets/db-cfg/db-name) > $@

.PHONY: psql
psql:
	${DOCKER} exec -it ghm-container-db psql -U $(shell cat secrets/db-user-root/username) $(shell cat secrets/db-cfg/db-name)

.PHONY: purge
purge:
	${DOCKER} rm $(CONTAINERS) -fv || true
	if [[ -v ROOT ]]
	then ${DOCKER} network rm ghm-net || true
	else ${DOCKER} pod rm -f ghm-pod || true
	fi
