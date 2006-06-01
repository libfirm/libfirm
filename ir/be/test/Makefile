include Makefile.config

COMPARES = $(addprefix compare_, $(filter-out $(DONTCOMPARE), $(SOURCES)))

.PHONY: all clean firm gcc compare

help:
	@echo "Targets:"
	@echo ""
	@echo "	gcc   	Build files with gcc"
	@echo "	firm	Build files with firm/edg"
	@echo "	compare	Compare results from firm with gcc executables"

all: compare

gcc: $(GCCEXES)

firm: $(FIRMEXES) $(FIRMASSEMBLERS)

compare: $(COMPARES)

gcc/%.result: gcc/%.exe
	@test -z gcc || mkdir -p gcc
	gcc/$*.exe >& $@ || true

firm/%.result: firm/%.exe
	@test -z firm || mkdir -p firm
	firm/$*.exe >& $@ || echo "$*.c" >> doesntrun.txt

compare_%.c: gcc/%.exe firm/%.exe gcc/%.result firm/%.result
	diff -u gcc/$*.result firm/$*.result || echo "$*.c" >> broken.txt

gcc/%.exe: %.c
	@test -z gcc || mkdir -p gcc
	$(GCC) $(GCC_CFLAGS) $*.c -o $@

firm/%.s: %.c
	@test -z firm || mkdir -p firm
	cd firm ; $(EDG) $(EDG_CFLAGS) ../$*.c || echo "$*.c" >> ../compile_failed.txt
	mv $*.s firm || echo "" > firm/$*.s

firm/%.exe: firm/%.s
	$(GCC) firm/$*.s -o $@ || echo "$*.c" >> link_failed.txt

clean:
	rm -f gcc/*
	rm -f firm/*
