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

gcc/%.s: %.c
	@test -z gcc || mkdir -p gcc
	@test -z $(RESDIR) || mkdir -p $(RESDIR)
	$(GCC) -c -S $(GCC_CFLAGS) $*.c -o $@

gcc/%.exe: %.c
	@test -z gcc || mkdir -p gcc
	@test -z $(RESDIR) || mkdir -p $(RESDIR)
	$(GCC) $(GCC_CFLAGS) $*.c -o $@

firm/%.s: %.c
	@mkdir -p firm
	@test -z $(RESDIR) || mkdir -p $(RESDIR)
	cd firm ; $(EDG) $(EDG_CFLAGS) ../$*.c || echo "$*.c" >> ../$(RESDIR)/compile_failed.txt
	mv $*.s firm

firm/%.exe: %.c
	@mkdir -p firm
	@mkdir -p $(RESDIR)
	$(EDG) $(EDG_CFLAGS) $*.c -o $@ || echo "$*.c" >> $(RESDIR)/link_failed.txt

icc/%.s: %.c
	@test -z icc || mkdir -p icc
	$(ICC) $(ICC_CFLAGS) -S $*.c -o $@

.PRECIOUS: icc/%.s

icc/%.exe: icc/%.s
	@test -z icc || mkdir -p icc
	$(ICC) $(ICC_CFLAGS) $*.c -o $@

clean:
	rm -f gcc/*
	rm -f firm/*
	rm -f $(RESDIR)/*
