.PHONY: all watch data serve import clean-db

all:
	dune build gui/{app.bc.js,index.html,bootstrap.min.css} data/chunks/*.sexp tools/combit.exe

data: tools/combit.ml data/combit.csv
	dune exec tools/combit.exe data/combit.csv data/chunks

watch:
	fd 'ml|dune' | entr -s 'make all'

serve:
	@echo ""
	@echo "Don't forget to start the containers:"
	@echo "cd db; make up"
	@echo ""
	source db/.env; export base_uri; caddy || true

clean-db:
	curl \
		-X DELETE "http://localhost:2015/api/customers" \
		-H "accept: application/json"

import: clean-db
	dune exec tools/combit.exe data/combit.csv | \
		curl \
		-X POST "http://localhost:2015/api/customers" \
		-H "accept: application/json" -H  "Prefer: return=none" \
		-H "Content-Type: application/json" \
		-d @-

clean:
	dune clean
