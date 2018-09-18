.PHONY: all watch

all: gui/data.sexp
	dune build gui/{app.bc.js,index.html,data.sexp}

gui/data.sexp: tools/combit.ml data/combit.csv
	dune exec tools/combit.exe < data/combit.csv > gui/data.sexp

watch:
	fd 'ml|dune' | entr -s 'make all'
