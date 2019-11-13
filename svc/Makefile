SHELL=/bin/bash

.PHONY: up
up:
	podman pod start ghm

.PHONY: down
down:
	podman pod stop ghm

.PHONY: dump.sql
dump.sql:
	touch $@
	chmod 600 $@
	podman exec ghm_db pg_dump -U $(shell cat secrets/db-user-root/username) $(shell cat secrets/db-cfg/db-name) > $@

.PHONY: psql
psql:
	podman exec -it ghm_db psql -U $(shell cat secrets/db-user-root/username) $(shell cat secrets/db-cfg/db-name)

.PHONY: purge
purge:
	podman pod rm -f ghm
