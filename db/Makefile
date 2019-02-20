SHELL=/bin/bash
compose=sudo docker-compose

.PHONY: up
up : .env postgrest.conf
	${compose} up -d

.env : env.sh
	./env.sh > .env

postgrest.conf :
	touch postgrest.conf

.PHONY: log
log :
	${compose} logs -f || true

.PHONY: down
down :
	${compose} down

.PHONY: init
init :
	./init.sh # generates postgrest.conf
	${compose} restart server

.PHONY: psql
psql : .env
	source .env ; ${compose} exec -e PGPASSWORD=$$pg_pwd db psql -U $$pg_user $$pg_db

.PHONY: purge
purge :
	${compose} down -v
	rm -f .env postgrest.conf

.PHONE: get-compose
get-compose:
	curl -L https://github.com/docker/compose/releases/download/1.20.1/docker-compose-`uname -s`-`uname -m` -o docker-compose
	chmod +x docker-compose
