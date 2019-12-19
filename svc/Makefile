SHELL=/bin/bash

.PHONY: up
up:
	podman pod start ghm-pod

.PHONY: down
down:
	podman pod stop ghm-pod

.PHONY: containers
containers:
	./containers.sh

.PHONY: containers-mount-webroot
containers-mount-webroot:
	MOUNT_WEBROOT=1 ./containers.sh

.PHONY: dump.sql
dump.sql:
	touch $@
	chmod 600 $@
	podman exec ghm-container-db pg_dump -U $(shell cat secrets/db-user-root/username) $(shell cat secrets/db-cfg/db-name) > $@

.PHONY: psql
psql:
	podman exec -it ghm-container-db psql -U $(shell cat secrets/db-user-root/username) $(shell cat secrets/db-cfg/db-name)

.PHONY: purge
purge:
	podman rm ghm-container-nginx -fv || true
	podman rm ghm-container-api -fv || true
	podman rm ghm-container-php -fv || true
	podman rm ghm-container-db -fv || true
	podman pod rm -f ghm-pod || true
