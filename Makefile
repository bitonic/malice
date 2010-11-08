default:
	ghc -o malice Parser.hs Semantics.hs Reduce.hs CodeCleanup.hs CodeGen.hs Malice.hs --make

clean:
	rm -f *.hi
	rm -f *.o
	rm -f malice