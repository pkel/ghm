.PHONY: all clean distclean dep

all:
	elm-make src/Main.elm --output elm.js

min: all
	closure-compiler --js elm.js > elm.min.js

cloc:
	cloc index.html src etc Makefile elm-package.json

dep:
	elm-package install

clean:
	rm -f elm.js elm.min.js

distclean: clean
	rm -rf elm-stuff
