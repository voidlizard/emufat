.DEFAULT: all

all:
	cabal-dev build
	rm -f ./FatGen
	rm -f ./TestVM
	ln -s dist/build/FatGen/FatGen
	ln -s dist/build/TestVM/TestVM
	cd cbits && make all
	cd cbits && make genfat 

clean:
	rm -f ./FatGen
	rm -f ./TestVM
	cabal-dev clean

config:
	cabal configure
	cabal-dev install-deps
	cabal-dev configure

test:
	./TestVM run cbits/teststubs

