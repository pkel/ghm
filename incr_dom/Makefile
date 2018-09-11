.PHONY: all watch

all:
	dune build {main.bc.js,index.html}

watch:
	fd 'ml|dune' | entr -s 'dune build {main.bc.js,index.html}'
