.PHONY: build clean distclean dependencies minify watch dev serve

build:
	npm run build

watch:
	npm run watch

serve:
	npm run serve

minify: all index.html
	closure-compiler -W QUIET --js elm.js > elm.min.js
	sed 's/elm\.js/elm\.min\.js/g' index.html > index.min.html

cloc:
	cloc index.html src etc Makefile elm-package.json

dependencies:
	npm install

clean:
	rm -f elm.js elm.min.js index.min.html -r elm-stuff/build-artifacts

distclean: clean
	rm -rf elm-stuff node_modules
