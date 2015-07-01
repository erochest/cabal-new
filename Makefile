
SRC=CabalNew.hs $(shell find CabalNew -name '*.hs')

all: init test docs package

init: stack.yaml

stack.yaml:
	stack init --prefer-nightly

test: build
	stack test

run: build
	stack exec -- cabal-new

lint:
	hlint CabalNew.hs CabalNew/*

tags: ${SRC}
	codex update

# docs:
# generate api documentation
#
# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.

install:
	stack install

# deploy:
# prep and push

clean:
	stack clean
	codex cache clean

distclean: clean

build:
	stack build

watch:
	ghcid "--command=stack ghci"

rebuild: clean build

.PHONY: all init test run clean distclean build rebuild watch
