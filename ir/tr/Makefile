# Hey, emacs, this is a -*- makefile -*-
#
# Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
# All rights reserved.
# Author: Goetz Lindenmaier
#
# Makefile for tr -- the type representation

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

DISTRIBMEMBERS	 = entity.m type.m typewalk.m
MEMBERS	 =  $(DISTRIBMEMBERS) mangle.m

CFILES = $(MEMBERS:.m=.c)

HFILES = $(MEMBERS:.m=.h)
HFILES +=  type_or_entity.h
HFILES +=  common.h debug.h gmp.h host.h ident.h irgraph.h  \
           irgwalk.h irmode.h irnode.h irop.h label.h misc.h \
           obst.h pset.h set.h tv.h xprintf.h xp_help.h entity_t.h

DISTRIB = $(DISTRIBMEMBERS:.m=.h)

OFILES = $(MEMBERS:%.m=../objects/%.o)

DFILES = $(MEMBERS:.m=.d)

TARGET = all

.PHONY: default all clean realclean install depend
.SUFFIXES: .d .h .c .o
.DEFAULT: $(TARGET)
.SILENT: $(DFILES) clean  # hides output

$(TARGET):      $(DFILES) $(OFILES)

%.d:    %.c
	$(SHELL) -ec '$(CXX) $(DEPENDFLAGS) $(INCLUDES) $(X_INCLUDES) $< | sed '\''s/\($*\)\.o[ :]*/\.\.\/objects\/\1.o $@ : /g'\'' > $@'


../objects/%.o:    %.c
	$(CXX) $(CFLAGS) $(CXXFLAGS) $(COPTFLAGS) $(INCLUDES) $(X_INCLUDES) -c $< -o $@

lib:
	cp $(DISTRIB) ../../include

clean:
	rm -f $(OFILES) $(DFILES)

realclean:	clean
	rm -f $(TARGET) *.flc TAGS

-include $(DFILES)
