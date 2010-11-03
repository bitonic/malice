default:
	rm -f Parser.hs
	rm -f Scanner.hs
	alex Scanner.x
	happy Parser.y
	ghc -o malice Scanner.hs Parser.hs CodeCleanup.hs Code2C.hs CodeGen.hs Semantics.hs Malice.hs

clean:
	rm -f Parser.hs
	rm -f Scanner.hs
	rm -f *.hi
	rm -f *.o
	rm -f malice