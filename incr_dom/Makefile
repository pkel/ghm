.PHONY: all watch

all:
	dune build gui/{app.bc.js,index.html} tools/combit.exe

watch:
	fd 'ml|dune' | entr -s 'make all'
