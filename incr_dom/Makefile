.PHONY: all watch

all:
	dune build gui/{app.bc.js,index.html}

watch:
	fd 'ml|dune' | entr -s 'make all'
