.PHONY: all watch

all: data/combit.sexp
	dune build gui/{app.bc.js,index.html} data/combit.sexp

data/combit.sexp: tools/combit.ml data/combit.csv
	dune exec tools/combit.exe < data/combit.csv > data/combit.sexp

watch:
	fd 'ml|dune' | entr -s 'make all'
