# Hey, emacs, this is a -*- makefile -*-
## Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
# All rights reserved.
# Author: Goetz Lindenmaier
#
# Makefile for adt -- abstract data types

CXX         = gcc
CFLAGS      = -pipe -Wall # -ansi -pedantic
CXXFLAGS    = # -g
COPTFLAGS   = -O3
LDFLAGS     =
DEPENDFLAGS = -M
LIBPATH     =
LIBS        =
X_LIBS      =
INCLUDES    = -I../adt -I../common -I../debug
X_INCLUDES  =

SHELL       = /bin/sh
MAKE        = /usr/bin/make

MEMBERS	 = array.m pdeq.m set.m

CFILES = $(MEMBERS:.m=.c)
CFIMES +=  xmalloc.c

HFILES = $(MEMBERS:.m=.h)
HFILES +=  cookies.h host.h obst.h pset.h
HFILES +=  misc.h tune.h debug.h

OFILES = $(MEMBERS:%.m=../objects/%.o)
OFILES += ../objects/xmalloc.o ../objects/pset.o

DFILES = $(MEMBERS:.m=.d)

TARGET = all

.PHONY: default all clean realclean install depend
.SUFFIXES: .d .h .c .o
.DEFAULT: $(TARGET)
.SILENT: $(DFILES) clean  # hides output

$(TARGET): $(DFILES) $(OFILES)

%.d:    %.c
	$(SHELL) -ec '$(CXX) $(DEPENDFLAGS) $(INCLUDES) $(X_INCLUDES) $< | sed '\''s/\($*\)\.o[ :]*/\.\.\/objects\/\1.o $@ : /g'\'' > $@'


../objects/%.o:    %.c
	$(CXX) $(CFLAGS) $(CXXFLAGS) $(COPTFLAGS) $(INCLUDES) $(X_INCLUDES) -c $< -o $@

# a hack to force recompilation
../objects/pset.o : set.c pset.h ../objects/set.o
	$(COMPILE.c) $(INCLUDES) -DPSET -o $@ $<
#	$(CXX) $(CFLAGS) $(CXXFLAGS) $(COPTFLAGS) $(INCLUDES) $(X_INCLUDES) -c $< -o $@

lib:

clean:
	rm -f $(OFILES) $(DFILES)

realclean:	clean
	rm -f $(TARGET) *.flc TAGS \#*
	rm -rf auto/


-include $(DFILES)
