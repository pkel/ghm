SHELL=/bin/bash
compose=podman

.PHONY: up
up : .env
	podman pod start ghm

.env : env.sh
	./env.sh > .env

.PHONY: down
down :
	podman pod stop ghm

.PHONY: dump.sql
dump.sql:
	touch $@
	chmod 600 $@
	source .env ; podman exec -i -e PGPASSWORD=$$db_root_pass ghm_db pg_dump -U $$db_root_user $$db_name > $@

.PHONY: psql
psql : .env
	source .env ; podman exec -i -e PGPASSWORD=$$db_root_pass ghm_db psql -U $$db_root_user $$db_name

.PHONY: purge
purge :
	podman pod rm -f ghm
