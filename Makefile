install: ylwrapper
	mv ylwrapper /usr/local/bin/

compile: ylwrapper.hs
	cabal update
	cabal install Network
	ghc -O2 -o ylwrapper ylwrapper.hs

uninstall:
	rm ylwrapper.hi ylwrapper.o
	rm /usr/local/bin/ylwrapper
