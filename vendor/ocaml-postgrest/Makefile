.PHONY: build
build:
	dune build

.PHONY: watch
watch:
	dune build -w

.PHONY: format
format:
	dune build @fmt --auto-promote

.PHONY: deps
deps: postgrest.opam postgrest-devel.opam
	opam install . --deps-only

.PHONY: clean
clean:
	rm -r _build

%.opam: dune-project
	dune build $@
