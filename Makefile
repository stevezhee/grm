all : Setup.exe dist/setup-config
	./Setup.exe build
	./Setup.exe install

Setup.exe : Setup.hs
	ghc --make -o $@ $<

dist/setup-config : grm.cabal
	./Setup.exe configure --user

clean :
	rm -f *.o *.hi
	rm -rf dist
