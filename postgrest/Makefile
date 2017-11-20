.PHONY: up
up :
	sudo docker-compose up -d

.PHONY: log
log :
	sudo docker-compose logs -f

.PHONY: down
down :
	sudo docker-compose down

.PHONY: reset
reset : clean init

.PHONY: init
init : up
	sudo docker-compose up -d postgres
	source ./env ; tools/wait-for-db
	source ./env ; psql -qf init.sql

.PHONY: import
import :
	source ./env ; psql -qf combit/import.sql

.PHONY: clean
clean :
	sudo docker-compose down -v
