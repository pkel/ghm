base_uri := $(shell source svc/.env; echo $$base_uri)

.PHONY: all watch format serve svc-up svc-init svc-psql import pwd clean-db clean

all:
	dune build app/app.bc.js tools/combit.exe
	cp _build/default/app/app.bc.js webroot/app.js

watch:
	fd 'ml|dune' | entr -s 'make all'

format:
	# do not auto promote test output
	dune runtest && dune build @fmt --auto-promote

serve:
	@echo ""
	@echo "Don't forget to start the containers:"
	@echo "make svc-up"
	@echo ""
	source svc/.env; export base_uri; caddy || true

svc-up:
	cd svc; make up

svc-init:
	cd svc; make init

svc-psql:
	cd svc; make psql

clean-db:
	curl \
		-X DELETE "$(base_uri)/api/customers" \
		-H "Authorization: Bearer $(shell scripts/get-token.sh)" \
		-H "Accept: application/json"

import: clean-db
	dune exec tools/combit.exe data/combit.csv | \
		curl \
		"${base_uri}/api/customers" \
		-H "Accept: application/json" -H  "Prefer: return=none" \
		-H "Content-Type: application/json" \
		-H "Authorization: Bearer $(shell scripts/get-token.sh)" \
		-d @-

clean:
	dune clean

deploy:
	# TODO: cannot work with new project layout
	cd _build/default/gui && rsync -a --delete index.html assets app.bc.js jhestia:/var/www/html/gui/
	rsync -a --delete letter jhestia:/var/www/html/
	rsync -a ./nginx-recipe jhestia:/etc/nginx/sites-available/default
	ssh jhestia "nginx -t && systemctl restart nginx"
