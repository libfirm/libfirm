# Hey, emacs, this is a -*- makefile -*-
#
# Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
# All rights reserved.
# Author: Goetz Lindenmaier
#
# Makefile for ident -- ??

CXX         = gcc
CFLAGS      = -pipe -Wall # -ansi -pedantic
CXXFLAGS    = # -g
COPTFLAGS   = -O3
LDFLAGS     =
DEPENDFLAGS = -M
LIBPATH     =
LIBS        =
X_LIBS      =
INCLUDES    = -I../adt -I../common -I../debug -I../ident
X_INCLUDES  =

SHELL       = /bin/sh
MAKE        = /usr/bin/make

MEMBERS	 = ident.m

CFILES = $(MEMBERS:.m=.c)

HFILES = $(MEMBERS:.m=.h)
HFILES += array.h cookies.h debug.h host.h misc.h set.h tune.h xprintf.h \
	  xp_help.h

OFILES = $(MEMBERS:%.m=../objects/%.o)

DFILES = $(MEMBERS:.m=.d)

TARGET = all

.PHONY: default all clean realclean install depend
.SUFFIXES: .d .h .c .o
.DEFAULT: $(TARGET)
.SILENT: $(DFILES) clean  # hides output

%.d:    %.c
	$(SHELL) -ec '$(CXX) $(DEPENDFLAGS) $(INCLUDES) $(X_INCLUDES) $< | sed '\''s/\($*\)\.o[ :]*/\.\.\/objects\/\1.o $@ : /g'\'' > $@'


../objects/%.o:    %.c
	$(CXX) $(CFLAGS) $(CXXFLAGS) $(COPTFLAGS) $(INCLUDES) $(X_INCLUDES) -c $< -o $@


$(TARGET):      $(DFILES) $(OFILES)

clean:
	rm -f $(OFILES) $(DFILES)

realclean:	clean
	rm -f $(TARGET) *.flc TAGS

-include $(DFILES)
