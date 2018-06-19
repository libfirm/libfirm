-include config.mak

LDFLAGS ?= -lm
CFLAGS ?= -Wall -g -O2 -fno-crossjumping -std=c99 -fPIC

LIBFIRM_ARCHIVE ?= ~/projects/firm/cparser/libfirm/build/debug/libfirm.a
CPPFLAGS ?= -I$(HOME)/projects/firm/cparser/libfirm/include/libfirm

all: ir2vhdl

ir2vhdl: $(LIBFIRM_ARCHIVE) ir2vhdl.o vhdlmodes.o firm2vhdl.o vhdlops.o lower-for-vhdl.o -lm

*.o: Makefile config.mak


# ifconv_hack.so: ifconv_hack.o
# 	gcc -shared ifconv_hack.o -o ifconf_hack.so

clean:
	rm -f *.o ir2vhdl *.vcg *.so
	cd test && $(MAKE) clean

test:
	cd test && $(MAKE) test -j1

.PHONY: test
