# Hey, emacs, this is a -*- makefile -*-
#
# Copyright (C) 1998 - 2001 by Universitaet Karlsruhe
# All rights reserved.
# Author: Goetz Lindenmaier
#
# Main Makefile

# This makefile makes firm and/or the testprograms.

SUBDIRS = ir testprograms

ifeq (,($MAKE))
MAKE = /usr/bin/make -k
endif

#MAKE = ${MAKE:-/usr/bin/make} #  Vorschlag Uwe
#MAKE = /usr/bin/make -k
#SHELL   = /bin/sh

.PHONY: default all clean realclean install depend ir testprograms

# Makes the intermediate representation and bundles it into a
# library
ir:
	$(MAKE) -C ir
# Makes the library, i.e., compiles and bundles the intermediate
# representation and extracts the necessary headers into directory
# include.
lib:	ir
	$(MAKE) -C ir lib

# Makes the testprograms.
testprograms:
	$(MAKE) -C testprograms

# Makes everything
all:	TAGS ir lib testprograms


clean:
	for i in $(SUBDIRS); do  $(MAKE) -C $$i clean; done

realclean:
	for i in $(SUBDIRS); do  $(MAKE) -C $$i realclean; done
	rm -f libfirm.a core include/*.h TAGS

TAGFILES = $(shell find . -name '*.c' -o -name '*.h')

TAGS:   $(TAGFILES)
	etags -C $(TAGFILES)
