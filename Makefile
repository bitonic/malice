GHCFLAGS = -Wall -O2 --make -fno-warn-missing-signatures -fno-warn-unused-do-bind

.PHONY : malice2asm
malice2asm: libmalice.o
	ghc ${GHCFLAGS} -o malice2asm Common.hs Parser.hs TypeCheck.hs OptimExpr.hs CGCommon.hs LLGen.hs CodeGen.hs Malice.hs


.PHONY : compile
compile: malice2asm

libmalice.o: libmalice.asm
	nasm -f elf "libmalice.asm"

.PHONY : clean
clean:
	rm -f malice_m2.tar.gz
	rm -f *.hi
	rm -f *.o
	rm -f malice2asm
	rm -f *~
	rm -rf autotest-results/

.PHONY : tar
tar:
	rm -f malice_m2.tar.gz
	tar zcvf malice_m2.tar.gz *.hs compile Makefile AUTHORS README

.PHONY : test
test: malice2asm
	./dotests.sh autotest/at-m2
	./dotests.sh autotest/at-m2extra
	./dotests.sh autotest/at-m2grad
	./dotests.sh autotest/at-m3
