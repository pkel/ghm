server=localhost:2015

.PHONY: all watch format serve import clean-db clean

all:
	dune build app/app.bc.js tools/combit.exe

watch:
	fd 'ml|dune' | entr -s 'make all'

format:
	# do not auto promote test output
	dune runtest && dune build @fmt --auto-promote

serve:
	@echo ""
	@echo "Don't forget to start the containers:"
	@echo "cd svc; make up"
	@echo ""
	source svc/.env; export base_uri; caddy || true

clean-db:
	# TODO: cannot work without jwt token
	curl \
		-X DELETE "http://${server}/api/customers" \
		-H "accept: application/json"

import: clean-db
	# TODO: cannot work without jwt token
	dune exec tools/combit.exe data/combit.csv | \
		curl \
		-X POST "http://${server}/api/customers" \
		-H "accept: application/json" -H  "Prefer: return=none" \
		-H "Content-Type: application/json" \
		-d @-

clean:
	dune clean

deploy:
	# TODO: cannot work with new project layout
	cd _build/default/gui && rsync -a --delete index.html assets app.bc.js jhestia:/var/www/html/gui/
	rsync -a --delete letter jhestia:/var/www/html/
	rsync -a ./nginx-recipe jhestia:/etc/nginx/sites-available/default
	ssh jhestia "nginx -t && systemctl restart nginx"
