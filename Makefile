default:
	ghc -o malice Parser.hs CodeCleanup.hs Code2C.hs CodeGen.hs Semantics.hs Malice.hs

clean:
	rm -f *.hi
	rm -f *.o
	rm -f malice