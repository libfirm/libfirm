


LIBNAME = libfirmjni.so

.PHONY: default all clean realclean depend



MEMBERS = Firm.m Firm.m Firm_common.m Dbginfo.m Ident.m Tv.m                 \
	Type.m Entity.m Type_or_entity.m Tpop.m Mangle.m 		\
	Irprog.m Irgraph.m Irnode.m Irmode.m Irop.m Ircons.m Ircgcons.m Irflag.m Irvrfy.m \
	Irdump.m Iropt.m Irgopt.m Ircgopt.m				\
	Irouts.m Irdom.m Irloop.m Cgana.m        			\
	Irgwalk.m Irgmod.m Typewalk.m Typegmod.m

OFILES  = $(MEMBERS:.m=.o)
HFILES  = $(MEMBERS:.m=.h)

all: 	lib

%.class: %.java
	javac $<

%.h:	%.class
	javah $(<:.class=)
	-cp Firm_0005fcommon.h Firm_common.h
	-cp Type_0005for_0005fentity.h Type_or_entity.h

%.o:	%.c %.h
	gcc $(CPPFLAGS) -c $< -o $@

lib:    $(HFILES) $(OFILES)
	gcc  -shared -fPIC -o $(LIBNAME) *.o /ben/goetz/proj/libfirm/i686/libfirm.a

clean:
#	rm -f *.class $(JHFILES) *.o $(JCFILES) $(JOFILES) $(CLSFILES) $(JFILES)

realclean: clean
#	rm -f *~ libfirmjni.a libfirmjni.so *.vcg
