server=localhost:2015

.PHONY: all watch format serve import clean-db clean

all: _build/default/letter
	dune build --profile release gui/{app.bc.js,index.html,assets/*/*} tools/combit.exe

watch:
	fd 'ml|dune' | entr -s 'make all'

format:
	# do not auto promote test output
	dune runtest && dune build @fmt --auto-promote

_build/default/letter:
	ln -s ../../letter _build/default/letter

serve:
	@echo ""
	@echo "Don't forget to start the containers:"
	@echo "cd db; make up"
	@echo ""
	source db/.env; export base_uri; caddy || true

clean-db:
	curl \
		-X DELETE "http://${server}/api/customers" \
		-H "accept: application/json"

import: clean-db
	dune exec tools/combit.exe data/combit.csv | \
		curl \
		-X POST "http://${server}/api/customers" \
		-H "accept: application/json" -H  "Prefer: return=none" \
		-H "Content-Type: application/json" \
		-d @-

clean:
	dune clean

deploy:
	scp -r _build/default/gui/(index.html|assets|app.bc.js) jhestia:/var/www/html/
