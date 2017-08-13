.PHONY: all clean distclean dep min watch

all:
	npm run build

watch:
	npm run watch

min: all index.html
	closure-compiler -W QUIET --js elm.js > elm.min.js
	sed 's/elm\.js/elm\.min\.js/g' index.html > index.min.html

cloc:
	cloc index.html src etc Makefile elm-package.json

dep:
	npm install
	elm-package install

clean:
	rm -f elm.js elm.min.js

distclean: clean
	rm -rf elm-stuff
