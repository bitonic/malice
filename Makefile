default:
	rm -f malice.hs
	rm -f scanner.hs
	alex scanner.x
	happy malice.y
	ghc -o malice scanner.hs malice.hs

clean:
	rm -f *.hs
	rm -f *.hi
	rm -f *.o
	rm -f malice