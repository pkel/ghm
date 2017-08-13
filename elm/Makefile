.PHONY: all clean distclean dep min

all:
	elm-make src/Main.elm --output elm.js

min: all index.html
	closure-compiler -W QUIET --js elm.js > elm.min.js
	sed 's/elm\.js/elm\.min\.js/g' index.html > index.min.html

cloc:
	cloc index.html src etc Makefile elm-package.json

dep:
	elm-package install

clean:
	rm -f elm.js elm.min.js

distclean: clean
	rm -rf elm-stuff
