SHELL=/bin/bash
compose=sudo docker-compose

.PHONY: up
up : postgrest.conf
	${compose} up -d

postgrest.conf : env postgrest.conf.sh
	./postgrest.conf.sh > postgrest.conf

.PHONY: log
log :
	${compose} logs -f || true

.PHONY: down
down :
	${compose} down

.PHONY: init
init : postgrest.conf
	./init.sh
	${compose} restart server

.PHONY: psql
psql :
	source ./env ; ${compose} exec -e PGPASSWORD=$$pg_pwd db psql -U $$pg_user $$pg_db

.PHONY: purge
purge :
	${compose} down -v

.PHONE: get-compose
get-compose:
	curl -L https://github.com/docker/compose/releases/download/1.20.1/docker-compose-`uname -s`-`uname -m` -o docker-compose
	chmod +x docker-compose
