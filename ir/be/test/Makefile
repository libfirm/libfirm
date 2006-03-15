#
#
#
RM   = rm -f
CAT  = cat
ECHO = echo
XCC ?= ~/local/bin/eccp
XCC_GOOD = gcc -malign-double -S


#
# generated files
#
LOG = run.log
ERR = compile.errors
ECNT = .errors
GCNT = .good
TESTERR = .compile.err.log
TESTSUC = .compile.suc.log

# maximum running time for a testprogram in seconds (default 5min)
maxtime ?= 300

#
# C-Generic backend:
# define extension and additional includes
#
#EXT = c.c
#INC = -I$(HOME)/cvs/be/C_GENERIC

#
# MMIX backend
#
#EXT = mms
#INC =

#
# ia32 backend
#
EXT = s
INC =

SRC   = apfel.c          Doit.c           LongLong.c     Switcher.c \
	Args.c           Empty.c          mul.c            Test.c \
	Arrays.c         EmptyFor.c       optest.c         Thilo.c \
	Field.c          Pdg.c            trivial_add.c \
	BinaryOpTest.c   Float.c          Queens.c         trivial_div.c \
	BreakTest.c      ForTest.c        QuickSort.c      trivial_empty.c \
	ByteTest.c       gcd.c            RegallocBug.c    trivial_extern_add.c \
	CallingTest.c    GlobalCseTest.c  Return.c         trivial_max.c \
	callref.c        Hanoi.c          SieveBits.c      trivial_sum_upto.c \
	HeapSort.c       Sieve.c          trivial_two_times.c \
	CondExpr.c       HelloWorld.c     SimpleExpr.c     While.c \
	IfExpr.c         Strings.c        ContinueTest.c   XXEndless.c \
	d.c              Int.c            struct.c \
	DeclTest.c       Label.c          structtest.c \
	Do.c             Local.c          Swap.c	   ll_call.c \
	MergeSort.c	 Or.c             while-printf.c   multidim-array.c \
	fehler1.c        fehler3.c        fehler4.c \
	fehler5.c        fehler6.c        fehler7.c        fehler8.c \
	fehler9.c        fehler10.c       fehler11.c       fehler12.c  \
	simd1.c	         simd2.c          enum.c           max.c \
	use_uninit_ptr.c func_arg.c       ns.c             array_type.c \
	sparam.c         wrong_cmp.c      nested_loops.c   alloca.c \
	nested_loops2.c  truth.c          iabs.c           stmt_expr.c \
	dblstruct.c      tailrec.c        types.c          types2.c \
	nullnode.c       bf_store.c       duffs.c          bf_init.c \
	bf_localinit.c   switch_test.c    rotate.c         biggest_prime.c \
	DivBug.c         strenght_red.c   divtest.c        pbqp_RedN.c \
	cmp.c



##################################
###### causes endless loop: ######
##################################


ASM_SRC = $(SRC:.c=.$(EXT))
TARGETS = $(ASM_SRC:.$(EXT)=)
LOGS    = $(ASM_SRC:.$(EXT)=.log)

.PHONY: run clean good good_i all
.SUFFIXES: .$(EXT)

all:
	@echo "compile   == run your backend"
	@echo "build     == assemble files (on target platform)"
	@echo "compare   == run the executeables and compare with GOOD results"
	@echo "good      == copy the log files into GOOD repository"
	@echo "run       == make the combined test (only for ia-32)"
	@echo "clean     == removes cores and logs"
	@echo "realclean == removes all generated files"
	@echo "test      == try to compile, used by automatic tests"


#
# how to assemble "something"
#
%:	%.$(EXT)
	@$(ECHO) gcc $(INC) -o $@ $<
	@gcc $(INC) -o $@ $< || gcc -o $@ error.c

#
# how to compile a C file into "something"
#
%.c.c:	%.c
	@$(ECHO) $(XCC) $<
	@$(XCC) $< >/dev/null 2>error || (mv core* $<.core ; $(ECHO) -e "\n$<: corefile $<.core" >> $(ERR) && $(CAT) error >> $(ERR) && $(ECHO) "int main(void) { printf(\"$<\\n  Compile failed!\\n\"); }" > $@)
	@if [ -f $@ ]; then $(RM) error ; else $(ECHO) "int main(void) { printf(\"$<\\n  Compile produced empty file!\\n\"); }" > $@ ;  $(CAT) error >> $(ERR) ; fi

%.s:	%.c
	@$(ECHO) $(XCC) $<
	@$(XCC) $< >/dev/null 2>error || (mv core* $<.core ; $(ECHO) -e "\n$<: corefile $<.core" >> $(ERR) && $(CAT) error >> $(ERR) && $(ECHO) "int main(void) { printf(\"$<\\n  Compile failed!\\n\"); }" > $@.c.c && gcc -S $@.c.c -o $@ && rm $@.c.c )
	@if [ -f $@ ]; then $(RM) error ; else $(ECHO) "int main(void) { printf(\"$<\\n  Compile produced empty file!\\n\"); }" > $@.c.c ; gcc -S $@.c.c -o $@; rm $@.c.c ;  $(CAT) error >> $(ERR) ; fi


#
# how to create an mmix emulator executable from a .mms assembler file (for gas)
#
%:	%.mms
	mmix-gcc -xassembler $< -o $@

#
# how to create a default executable from an .s assembler file (for gas)
#
%:	%.c.c
	gcc -malign-double -g -lm $< -o $@ || true

#
# how to create a default executable from an .s assembler file (for gas)
#
%:	%.s
	gcc -g -lm $< -o $@ || true

#
# how to create a file from a target
#
%.log:	%
	@echo "running: $^"
	@ulimit -St $(maxtime); ./$^ > $@ || \
	if [ "$$?" = "152" ]; then echo "$^: Process killed (CPU time limit exceeded)"; tail -1000 $@ > $@.tmp; mv $@.tmp $@; fi || \
	true

$(TARGETS):	$(ASM_SRC)

compile: clean $(ASM_SRC)

build: $(TARGETS)

run:	error.c $(TARGETS)
	@$(RM) $(LOG)
	@$(ECHO) -e "\nRunning..."
	@for i in $(TARGETS); do ./$$i | tee -a $(LOG) | tee $$i.log ; done
	@if [ -s $(ERR) ]; then $(ECHO) -e "\nThere were compile errors, see $(LOG)"; fi
	@$(ECHO) -e "\nError reports:" >> $(LOG)
	-@$(CAT) $(ERR) >> $(LOG)
	@$(RM) $(ERR)
	@$(ECHO) "For run log, see file $(LOG)"

error.c:
	@$(ECHO) "main() { printf(\"GCC compile errors!\n\"); }" > $@

clean:
	$(RM) *.stat *.ilp *.ra *.ra.res *.pstat *.vcg *.$(EXT) *.o core* $(TARGETS) .error.c *.errlog $(TESTERR) $(TESTSUC) error $(ERR) .comp.log

realclean:	clean
	$(RM) $(LOG) $(RUN) $(ERR) $(ECNT) $(GCNT) *.core *.log GOOD/*.good

runonly:	$(TARGETS)
	@for i in $(TARGETS); do ./$$i ; done

good_i: $(LOGS)
	@for i in $(TARGETS); do mv $$i.log GOOD/$$i.good ; done

good:
	$(MAKE) XCC="$(XCC_GOOD)" realclean good_i

compare: $(LOGS)
	echo > $(ECNT)
	echo > $(GCNT)
#	@for i in $(TARGETS); do diff -u1 $$i.log GOOD/$$i.good | less ; done
	@for i in $(TARGETS); do echo $$i >> $(GCNT); diff -u1 $$i.log GOOD/$$i.good || echo $$i >> $(ECNT) ; done
	@diff -u $(ECNT) $(GCNT) || true
	@echo "############# Number of Differences #############"
	@wc -w $(ECNT)
	@echo "############# Total number of Tests #############"
	@wc -w $(GCNT)
	@echo "############# Detailed list of failures #########"
	@$(RM) $(ECNT) $(GCNT)

test: clean
	for i in $(SRC); do \
		$(ECHO) $(XCC) $$i; \
		$(XCC) $$i &> $$i.errlog; \
		if [ $$? -ne 0 ]; then \
			$(ECHO) -e "Compilation of $$i failed: \n" >> $(TESTERR); \
			$(CAT) $$i.errlog >> $(TESTERR); \
			$(ECHO) -e "\n--------------------------------------------------------------------------------------\n" >> $(TESTERR); \
		else \
			$(ECHO) "Compilation of $$i successful." >> $(TESTSUC); \
		fi \
	done

xml1:
	for i in *.c.xml ; do ~/CRS/firmxml/test/inout $$i $$i.xml ; done

xml2:
	for i in *.c.xml.xml ; do ~/CRS/firmxml/test/inout $$i $$i.xml ; done

xml3:
	for i in *.c.xml.xml.xml ; do ~/CRS/firmxml/test/inout $$i $$i.xml ; done
