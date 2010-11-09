default:
	ghc -Wall -O2 --make -o malice Parser.hs Semantics.hs Reduce.hs CodeCleanup.hs CodeGen.hs Malice.hs

clean:
	rm -f *.hi
	rm -f *.o
	rm -f malice