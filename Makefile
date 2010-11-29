GHCFLAGS = -Wall -O2 --make -fno-warn-missing-signatures

.PHONY : malice2asm
malice2asm:
	ghc ${GHCFLAGS} -o malice2asm Parser.hs Semantics.hs OptimExpr.hs LLGen.hs CodeGen.hs Malice.hs


.PHONY : compile
compile: malice2asm


.PHONY : clean
clean:
	rm -f malice_m2.tar.gz
	rm -f *.hi
	rm -f *.o
	rm -f malice2asm

.PHONY : tar
tar:
	rm -f malice_m2.tar.gz
	tar zcvf malice_m2.tar.gz *.hs compile Makefile AUTHORS README

.PHONY : test
test: malice2asm
	./dotests.sh autotest/at-m2
	./dotests.sh autotest/at-m2extra
	./dotests.sh autotest/at-m2grad
