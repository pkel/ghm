.PHONY: all clean distclean dep

all:
	elm-make src/Main.elm --output elm.js 2>&1 | vimpager

min: all
	closure-compiler --js elm.js > elm.min.js

dep:
	elm-package install

clean:
	rm -f elm.js elm.min.js

distclean: clean
	rm -rf elm-stuff
