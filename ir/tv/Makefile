# Hey, emacs, this is a -*- makefile -*-
#
# Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
# All rights reserved.
# Author: Goetz Lindenmaier
#
# Makefile for tv -- constant table and machine arithmetic
CXX         = gcc
CFLAGS      = -pipe -Wall # -ansi -pedantic
CXXFLAGS    = # -g
COPTFLAGS   = -O3
LDFLAGS     =
DEPENDFLAGS = -M
LIBPATH     =
LIBS        =
X_LIBS      =
INCLUDES    = -I../adt -I../common -I../debug -I../ident -I../include \
	      -I../ir -I../tr -I../tv
X_INCLUDES  =

SHELL       = /bin/sh
MAKE        = /usr/bin/make

DISTRIBMEMBERS	 = tv.m
MEMBERS	 =  $(DISTRIBMEMBERS)

CFILES = $(MEMBERS:.m=.c)

HFILES = $(MEMBERS:.m=.h)
HFILES += ieee754.h

HFILES += common.h debug.h entity.h gmp.h ident.h irmode.h obst.h \
	  pset.h tune.h type.h xprintf.h xp_help.h

DISTRIB = $(DISTRIBMEMBERS:.m=.h)

OFILES = $(MEMBERS:%.m=../objects/%.o)

DFILES = $(MEMBERS:.m=.d)

TARGET = all

.PHONY: default all clean realclean install depend
.SUFFIXES: .d .h .c .o
.DEFAULT: $(TARGET)
.SILENT: $(DFILES) clean  # hides output

%.d:    %.c
	$(SHELL) -ec '$(CXX) $(DEPENDFLAGS) $(INCLUDES) $(X_INCLUDES) $< | sed '\''s/$*\\.o[ :]*/& $@/g'\'' > $@'


../objects/%.o:    %.c
	$(CXX) $(CFLAGS) $(CXXFLAGS) $(COPTFLAGS) $(INCLUDES) $(X_INCLUDES) -c $< -o $@

lib:
	cp $(DISTRIB) ../../include

$(TARGET):      $(DFILES) $(OFILES)

clean:
	rm -f $(OFILES) $(DFILES)

realclean:	clean
	rm -f $(TARGET) *.flc TAGS

-include $(DFILES)
