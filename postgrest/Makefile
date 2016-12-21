database = ghm-postgrest

.PHONY : db combit run all

all : run

run :
	postgrest etc/postgrest.cfg

db :
	dropdb --if-exists $(database)
	createdb $(database)
	psql $(database) -qf schema/internal.sql
	psql $(database) -qf schema/api.sql
	psql $(database) -qf etc/ghm-cfg.sql

combit : db
	psql $(database) -qf combit/import.sql

clean :
	# rm -rf schema.png
