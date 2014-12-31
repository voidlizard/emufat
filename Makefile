.DEFAULT: all
.PHONY: config


all:
	cabal build
	cd cbits && make all
	cd cbits && make genfat 

clean:
	cabal clean

config:
	cabal sandbox init
	cabal install --dependencies-only
	cabal configure

test:
	dist/build/TestVM/TestVM run cbits/teststubs

