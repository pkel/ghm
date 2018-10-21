.PHONY: all watch format serve import clean-db clean

all:
	dune build gui/{app.bc.js,index.html,assets/*/*} tools/combit.exe

watch:
	fd 'ml|dune' | entr -s 'make all'

format:
	# do not auto promote test output
	dune runtest && dune build @fmt --auto-promote

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
