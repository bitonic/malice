default:
	rm -f Malice.hs
	rm -f Scanner.hs
	alex Scanner.x
	happy Malice.y
	ghc -o malice Scanner.hs Malice.hs

clean:
	rm -f *.hs
	rm -f *.hi
	rm -f *.o
	rm -f malice