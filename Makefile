
SRC=CabalNew.hs $(shell find CabalNew -name '*.hs')

all: init test docs package

init:
	cabal sandbox init
	make deps

test: build

run:
	cabal run

lint:
	hlint CabalNew.hs CabalNew/*

tags: ${SRC}
	hasktags --ctags CabalNew.hs CabalNew

# docs:
# generate api documentation
#
# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.

install:
	cabal install

# deploy:
# prep and push

clean:
	cabal clean

distclean: clean
	cabal sandbox delete

configure: clean
	cabal configure --enable-tests

deps: clean
	cabal install --only-dependencies --allow-newer --enable-tests
	cabal configure --enable-tests

build:
	cabal build

rebuild: clean configure build

.PHONY: all init test run clean distclean configure deps build rebuild