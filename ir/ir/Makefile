# Hey, emacs, this is a -*- makefile -*-
#
# Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
# All rights reserved.
# Author: Goetz Lindenmaier
#
# Makefile for ir -- intermediate representation of the program

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

DISTRIBMEMBERS = ircons.m irdump.m irflag.m irgmod.m irgraph.m irgopt.m \
           irgwalk.m irmode.m irnode.m irop.m iropt.m irprog.m irvrfy.m
MEMBERS = $(DISTRIBMEMBERS)

CFILES = $(MEMBERS:.m=.c)

HFILES = $(MEMBERS:.m=.h)
HFILES +=  irnode_t.h irgraph_t.h
HFILES +=  array.h common.h cookies.h debug.h entity.h gmp.h  \
           host.h ident.h label.h misc.h obst.h pdeq.h pset.h \
           set.h tune.h tv.h type.h xprintf.h xp_help.h irnode2.h

DISTRIB = $(DISTRIBMEMBERS:.m=.h)

OFILES = $(MEMBERS:%.m=../objects/%.o)

DFILES = $(MEMBERS:.m=.d)

TARGET = all

.PHONY: default all clean realclean install depend
.SUFFIXES: .d .h .c .o
.DEFAULT: $(TARGET)
.SILENT: $(DFILES) clean  # hides output


$(TARGET): $(DFILES) $(OFILES)

# the preprocessor computes the dependencies and saves them to a file
# the sed script adds the path to ../objects and file.d to the dependency.
%.d:	%.c
	$(SHELL) -ec '$(CXX) $(DEPENDFLAGS) $(INCLUDES) $(X_INCLUDES) $< | sed '\''s/\($*\)\.o[ :]*/\.\.\/objects\/\1.o $@ : /g'\'' > $@'


../objects/%.o:	%.c
	$(CXX) $(CFLAGS) $(CXXFLAGS) $(COPTFLAGS) $(INCLUDES) $(X_INCLUDES) -c $< -o $@

lib:
	cp $(DISTRIB) ../../include

clean:
	rm -f $(OFILES) $(DFILES)

realclean:	clean
	rm -f $(TARGET) *.flc TAGS

-include $(DFILES)
