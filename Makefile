# Hey, emacs, this is a -*- makefile -*-
#
# Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
# All rights reserved.
# Author: Goetz Lindenmaier
#
# Main Makefile

# This makefile makes firm and/or the testprograms.

SUBDIRS = ir testprograms

MAKE    = /usr/bin/make -k
SHELL   = /bin/sh

.PHONY: default all clean realclean install depend ir testprograms


ir:
	$(MAKE) -C ir

all:	TAGS ir testprograms

testprograms:
	$(MAKE) -C testprograms

lib:	ir
	$(MAKE) -C ir lib

clean:
	for i in $(SUBDIRS); do  $(MAKE) -C $$i clean; done

realclean:
	for i in $(SUBDIRS); do  $(MAKE) -C $$i realclean; done
	rm -f libfirm.a core include/*.h TAGS

TAGFILES = $(shell find . -name '*.c' -o -name '*.h')

TAGS:   $(TAGFILES)
	etags -C $(TAGFILES)
