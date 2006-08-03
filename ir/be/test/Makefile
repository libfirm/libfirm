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
	@test -z $(RESDIR) || mkdir -p $(RESDIR)
	firm/$*.exe >& $@ || echo "$*.c" >> $(RESDIR)/run_failed.txt

compare_%.c: gcc/%.exe firm/%.exe gcc/%.result firm/%.result
	@test -z $(RESDIR) || mkdir -p $(RESDIR)
	diff -u gcc/$*.result firm/$*.result || echo "$*.c" >> $(RESDIR)/compare_failed.txt

gcc/%.exe: %.c
	@test -z gcc || mkdir -p gcc
	@test -z $(RESDIR) || mkdir -p $(RESDIR)
	$(GCC) $(GCC_CFLAGS) $*.c -o $@

firm/%.s: %.c
	@test -z firm || mkdir -p firm
	@test -z $(RESDIR) || mkdir -p $(RESDIR)
	cd firm ; $(EDG) $(EDG_CFLAGS) ../$*.c || echo "$*.c" >> ../$(RESDIR)/compile_failed.txt
	mv $*.s firm || echo "" > firm/$*.s

firm/%.exe: firm/%.s
	@test -z $(RESDIR) || mkdir -p $(RESDIR)
	$(GCC) $(ASM_FLAGS) firm/$*.s -o $@ || echo "$*.c" >> $(RESDIR)/link_failed.txt

clean:
	rm -f gcc/*
	rm -f firm/*
	rm -f $(RESDIR)/*
