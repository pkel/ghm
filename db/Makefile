.PHONY: up
up :
	sudo docker-compose up -d

.PHONY: log
log :
	sudo docker-compose logs -f || true

.PHONY: down
down :
	sudo docker-compose down

.PHONY: init
init :
	source ./.env ; sudo docker-compose exec -e PGPASSWORD=$$pg_pwd -T db psql -U $$pg_user $$pg_db < init.sql
	sudo docker-compose restart server

.PHONY: purge
purge :
	sudo docker-compose down -v
