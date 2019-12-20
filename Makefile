DOCKER ?= podman

.PHONY: all
all:
	dune build @default
	cp _build/install/default/share/ghm/app.js webroot/app.js

.PHONY: opt
opt:
	dune build --profile release @app/install
	cp _build/install/default/share/ghm/app.js webroot/app.js

.PHONY: watch
watch:
	fd 'ml|dune' | entr -s 'make all'

.PHONY: static
static:
	${DOCKER} pull ocaml/opam2:alpine-3.10-ocaml-4.07
	${DOCKER} build -t ghm-image-ocaml -f static-Dockerfile .
	${DOCKER} run -it --userns=keep-id -v $(shell pwd):/src:z ghm-image-ocaml

.PHONY: format
format:
	# do not auto promote test output
	dune runtest && dune build @fmt --auto-promote

.PHONY: up
up:
	cd svc; make up

.PHONY: down
down:
	cd svc; make down

.PHONY: psql
psql:
	cd svc; make psql

.PHONY: psql
clean:
	dune clean

.PHONY: psql
deploy-webroot: opt
	rsync -a --delete --info=progress2 svc/webroot/ hestia:ghm/webroot/
