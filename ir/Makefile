# Hey, emacs, this is a -*- makefile -*-
#
# Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
# All rights reserved.
# Author: Goetz Lindenmaier
#
# Makefile for IR library.

# This makefile descends to all directories listed in INCLUDES and to
# testprograms and calls make with the corresponding target there.
# After returning it builds a library out of the firm files.
# Further it updates the TAGS file with each recompilation

SUBDIRS = adt common debug ident ir tr tv
INCLUDES = -I../adt -I../common -I../debug -I../ident -I../include \
	   -I../ir -I../tr -I../tv
OFILES =

AR          = ar rcs

MAKE    = /usr/bin/make -k
SHELL   = /bin/sh

CXX         = gcc
CFLAGS      = -pipe -Wall # -ansi -pedantic # -g
COPTFLAGS   = -O3 -ffast-math # -fno-caller-saves

TARGET	= libfirm.a

.PHONY: default all clean realclean install depend $(TARGET)

$(TARGET):
#	mkdir objects
	for i in $(SUBDIRS); do $(MAKE) "INCLUDES=$(INCLUDES)" "CXXFLAGS=-g" -C $$i; done
	$(AR) $(TARGET) objects/*.o
	mv $(TARGET) ..

debug:
	for i in $(SUBDIRS); do $(MAKE) "CFLAGS=$(CFLAGS) -g INCLUDES=$(INCLUDES)" -C $$i; done

all:	TAGS $(TARGET)

#testprograms:
#	$(MAKE) -C testprograms

clean:
	for i in $(SUBDIRS); do  $(MAKE) -C $$i clean; done

realclean:
	for i in $(SUBDIRS); do  $(MAKE) -C $$i realclean; done
	$(MAKE) -C testprograms realclean
	rm -f $(TARGET) core

profile:        realclean
	for i in $(SUBDIRS); do  \
                        $(MAKE) \
                        "CXX=$(CXX)" \
                        "CFLAGS=-g -pg" \
			"INCLUDES=$(INCLUDES)" -C $$i; \
        done


TAGFILES = $(shell find . -name '*.c' -o -name '*.h')

TAGS:   $(TAGFILES)
	etags -C $(TAGFILES)
