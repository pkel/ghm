.PHONY: all clean distclean dep

all:
	elm-make src/Main.elm --output elm.js 2>&1 | vimpager

dep:
	elm-package install

clean:
	rm -f elm.js

distclean: clean
	rm -rf elm-stuff
