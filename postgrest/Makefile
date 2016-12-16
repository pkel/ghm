database = ghm-postgrest

.PHONY : db combit run png all

all : run

run :
	postgrest postgrest.cfg

db : png
	dropdb --if-exists $(database)
	createdb $(database)
	psql $(database) -qf schema.sql
	psql $(database) -qf config.sql

combit : db
	psql $(database) -qf combit/import.sql

png : schema.png

clean :
	rm -rf schema.png

schema.png : schema.sql
	# sqlt-graph -f PostgreSQL -o schema.png -t png schema.sql
