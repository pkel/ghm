.PHONY: all watch

all:
	opam exec dune build {main.bc.js,index.html}

watch:
	fd 'ml|dune' | entr -s 'make all'
