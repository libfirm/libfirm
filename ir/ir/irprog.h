/* Copyright (C) 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Goetz Lindenmaier
**
** irprog.h: ir representation of a program
**
** This file defines a construct that keeps all information about a
** program:
**   - A list of all procedures.
**   - A list of all types.
**   - A global type that can be thought of as a god-class containing all
**     global variables and procedures.  This is not the base class of
**     all classes in a class hierarchy (as, e.g., "object" in java).
**   - (An obstack containing global things, e.g., the above mentioned lists.)
*/

# ifndef _IRPROG_H_
# define _IRPROG_H_

# include "irnode.h"
# include "type.h"

struct ir_prog {
  firm_kind kind;
  ir_graph *main_irg;              /* entry point to the compiled program */
                  /* or a list, in case we compile a library or the like? */
  ir_graph **graphs;               /* all graphs in the ir */
  type_class *glob_type;           /* global type.  Class as it can have
				      fields and procedures.  Does this work?
				      Better name??? @@@ */
  type **types;                    /* all types in the ir */
  /*struct obstack *obst;	    * @@@ Should we place all types and
                                      entities on an obstack, too? */
#ifdef DEBUG_libfirm
  long max_node_nr;                /* to generate unique numbers for nodes. */
#endif
};

typedef struct ir_prog ir_prog;

/* A variable from where everything in the ir can be accessed. */
ir_prog *irp;


/* initializes ir_prog. Calles the constructor for an ir_prog. */
void init_irprog(void);

/* Creates a new ir_prog, returns it and sets irp with it.
   Automatically called by init_firm through init_irprog. */
ir_prog *new_ir_prog (void);

/* Access the main routine of the compiled program. */
ir_graph *get_irp_main_irg();
void      set_irp_main_irg(ir_graph *main_irg);

/* Adds irg to the list of ir graphs in irp. */
void      add_irp_irg(ir_graph *irg);
int       get_irp_n_irgs();
ir_graph *get_irp_irg(int pos);
void      set_irp_irg(int pos, ir_graph *irg);

/* Adds type to the list of types in irp. */
void  add_irp_type(type *typ);
int   get_irp_n_types();
type *get_irp_type(int pos);
void  set_irp_type(int pos, type *typ);

/** Functions to access the fields of ir_prog **/
type_class *get_glob_type(void);

#ifdef DEBUG_libfirm
/* Returns a new, unique number to number nodes or the like. */
int get_irp_new_node_nr();
#endif

#endif /* ifndef _IRPROG_H_ */
