.PHONY: all watch data

all:
	dune build gui/{app.bc.js,index.html,bootstrap.min.css} data/chunks/*.sexp

data: tools/combit.ml data/combit.csv
	dune exec tools/combit.exe data/combit.csv data/chunks

watch:
	fd 'ml|dune' | entr -s 'make all'

serve:
	@echo ""
	@echo "Don't forget to start the containers:"
	@echo "cd db; make up"
	@echo ""
	source db/.env; export base_uri; caddy || true

clean:
	dune clean
