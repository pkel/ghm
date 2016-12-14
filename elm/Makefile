.PHONY: all clean distclean

all:
	elm-make src/Main.elm --output elm.js 2>&1 | vimpager

clean:
	rm -f elm.js

distclean: clean
	rm -rf elm-stuff
