# deprecated
# base_uri := $(shell source svc/.env; echo $$base_uri)

.PHONY: all watch format serve svc-up svc-init svc-psql import pwd clean-db clean

all:
	dune build @install
	cp _build/install/default/share/ghm/app.js webroot/app.js

opt:
	dune build --profile release @app/install
	cp _build/install/default/share/ghm/app.js webroot/app.js

watch:
	fd 'ml|dune' | entr -s 'make all'

format:
	# do not auto promote test output
	dune runtest && dune build @fmt --auto-promote

up:
	cd svc; make up

down:
	cd svc; make down

psql:
	cd svc; make psql

clean-db:
	# deprecated
	false
	curl \
		-X DELETE "$(base_uri)/api/customers" \
		-H "Authorization: Bearer $(shell scripts/get-token.sh)" \
		-H "Accept: application/json"

import: clean-db
	# deprecated
	false
	dune exec tools/combit.exe data/combit.csv | \
		curl \
		"${base_uri}/api/customers" \
		-H "Accept: application/json" -H  "Prefer: return=none" \
		-H "Content-Type: application/json" \
		-H "Authorization: Bearer $(shell scripts/get-token.sh)" \
		-d @-

clean:
	dune clean

deploy-webroot: opt
	rsync -a --delete --info=progress2 webroot/ hestia:ghm/webroot/
