GHCFLAGS = -Wall -O2 --make -fno-warn-missing-signatures -fno-warn-unused-do-bind
default:
	ghc ${GHCFLAGS} -o malice Parser.hs Semantics.hs Reduce.hs CodeCleanup.hs CodeGen.hs Malice.hs

clean:
	rm -f *.hi
	rm -f *.o
	rm -f malice