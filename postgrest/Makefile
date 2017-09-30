.PHONY: run
run :
	sudo docker-compose up

.PHONY: reset
reset : clean init

.PHONY: init
init :
	sudo docker-compose up -d postgres
	source ./env ; tools/wait-for-db
	source ./env ; psql -qf schema/public.sql

.PHONY: combit
combit :
	sudo docker-compose up -d postgres
	source ./env ; psql -qf combit/import.sql

.PHONY: clean
clean :
	sudo docker-compose down -v
