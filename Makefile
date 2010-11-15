GHCFLAGS = -Wall -O2 --make -fno-warn-missing-signatures -fno-warn-unused-do-bind

malice2asm:
	ghc ${GHCFLAGS} -o malice2asm Parser.hs Semantics.hs LLGen.hs CodeGen.hs Malice.hs


.PHONY : compile
compile: malice2asm


.PHONY : clean
clean:
	rm -f malice_ms2.tar.gz
	rm -f *.hi
	rm -f *.o
	rm -f malice2asm

.PHONY : tar
tar:
	rm -f malice_ms2.tar.gz
	tar zcvf malice_ms2.tar.gz *.hs compile Makefile AUTHORS
