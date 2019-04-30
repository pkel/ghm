base_uri := $(shell source svc/.env; echo $$base_uri)

.PHONY: all watch format serve svc-up svc-init svc-psql import pwd clean-db clean

all:
	dune build app/app.bc.js tools/combit.exe
	cp _build/default/app/app.bc.js webroot/app.js

opt:
	dune build --profile release app/app.bc.js
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

psql:
	source svc/.env; PGPASSWORD=$$db_root_pass psql -h localhost -p 5432 -U $$db_root_user ghm

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
	ssh jhestia 'mkdir -p ghm'
	rsync -a --delete --info=progress2 --exclude=vendor --exclude=_* --exclude=repo --delete-excluded ./ jhestia:ghm/
