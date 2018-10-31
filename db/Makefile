SHELL=/bin/bash
compose=sudo docker-compose

.PHONY: up
up :
	${compose} up -d

.PHONY: log
log :
	${compose} logs -f || true

.PHONY: down
down :
	${compose} down

.PHONY: init
init :
	source ./.env ; ${compose} exec -e PGPASSWORD=$$pg_pwd -T db psql -U $$pg_user $$pg_db < init.sql
	${compose} restart server

.PHONY: psql
psql :
	source ./.env ; ${compose} exec -e PGPASSWORD=$$pg_pwd db psql -U $$pg_user $$pg_db

.PHONY: purge
purge :
	${compose} down -v

.PHONE: get-compose
get-compose:
	curl -L https://github.com/docker/compose/releases/download/1.20.1/docker-compose-`uname -s`-`uname -m` -o docker-compose
	chmod +x docker-compose
