DIRS = adt common debug ident ir tr tv
INCDIRS = -I../include -I../ir/include

LIBDIRS=-L.. -L/usr/public/lib/gmp/

LIBS=-lfirm -lgmp

%.o:	%.c ../libfirm.a
	gcc -c $(INCDIRS) -g $<

all: empty const_eval_example 					\
     if_example if_else_example if_while_example cond_example	\
     call_str_example memory_example array-stack_example	\
     array-heap_example oo_program_example inheritance_example	\
     irr_cf_example	\
     irr_loop_example dead_block_example global_var_example	\
     three_cfpred_example

clean:
	rm -f 					\
	empty const_eval_example 		\
	if_example if_else_example 		\
	if_while_example cond_example 		\
	call_str_example memory_example 	\
	array-stack_example array-heap_example	\
	oo_program_example inheritance_example	\
	irr_cf_example	\
	irr_loop_example dead_block_example	\
	global_var_example three_cfpred_example	\
	*.o *.vcg core

realclean:	clean
	rm -f \#* *.flc
	rm -rf auto/

run:
	./empty; 		\
	const_eval_example;	\
	if_example; 		\
	if_else_example; 	\
	if_while_example; 	\
	cond_example; 		\
	call_str_example; 	\
	memory_example;		\
	array-stack_example;	\
	array-heap_example; 	\
	oo_program_example;	\
	inheritance_example;	\
	irr_cf_example;		\
	irr_loop_example;	\
	dead_block_example;	\
	global_var_example;	\
	three_cfpred_example;

empty: empty.o
	gcc -o empty empty.o $(LIBDIRS) $(LIBS)

const_eval_example: const_eval_example.o
	gcc -o const_eval_example const_eval_example.o $(LIBDIRS) $(LIBS)

if_example: if_example.o
	gcc -o if_example if_example.o $(LIBDIRS) $(LIBS)

if_else_example: if_else_example.o
	gcc -o if_else_example if_else_example.o $(LIBDIRS) $(LIBS)

if_while_example: if_while_example.o
	gcc -o if_while_example if_while_example.o $(LIBDIRS) $(LIBS)

cond_example: cond_example.o
	gcc -o cond_example cond_example.o $(LIBDIRS) $(LIBS)

call_str_example: call_str_example.o
	gcc -o call_str_example call_str_example.o $(LIBDIRS) $(LIBS)

memory_example: memory_example.o
	gcc -o memory_example memory_example.o $(LIBDIRS) $(LIBS)

array-stack_example: array-stack_example.o
	gcc -o array-stack_example array-stack_example.o $(LIBDIRS) $(LIBS)

array-heap_example: array-heap_example.o
	gcc -o array-heap_example array-heap_example.o $(LIBDIRS) $(LIBS)

oo_program_example: oo_program_example.o
	gcc -o oo_program_example oo_program_example.o $(LIBDIRS) $(LIBS)

inheritance_example: inheritance_example.o
	gcc -o inheritance_example inheritance_example.o $(LIBDIRS) $(LIBS)

irr_cf_example: irr_cf_example.o
	gcc -o irr_cf_example irr_cf_example.o $(LIBDIRS) $(LIBS)

irr_loop_example: irr_loop_example.o
	gcc -o irr_loop_example irr_loop_example.o $(LIBDIRS) $(LIBS)

dead_block_example: dead_block_example.o
	gcc -o dead_block_example dead_block_example.o $(LIBDIRS) $(LIBS)

global_var_example: global_var_example.o
	gcc -o global_var_example global_var_example.o $(LIBDIRS) $(LIBS)

three_cfpred_example: three_cfpred_example.o
	gcc -o three_cfpred_example three_cfpred_example.o $(LIBDIRS) $(LIBS)
