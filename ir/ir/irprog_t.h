
/* $Id$ */

# ifndef _IRPROG_T_H_
# define _IRPROG_T_H_

#include "irprog.h"
#include "common_t.h"

struct ir_prog {
  firm_kind kind;
  ir_graph  *main_irg;            /* entry point to the compiled program */
                  /* or a list, in case we compile a library or the like? */
  ir_graph **graphs;              /* all graphs in the ir */
  type      *glob_type;           /* global type.  Must be a class as it can
				     have fields and procedures.  */
  type     **types;               /* all types in the ir */
  ir_graph  *const_code_irg;      /* This ir graph gives the proper environment
				     to allocate nodes the represent values
				     of constant entities. It is not meant as
				     a procedure.  */
  /*struct obstack *obst;	   * @@@ Should we place all types and
                                     entities on an obstack, too? */

#ifdef DEBUG_libfirm
  long max_node_nr;                /* to generate unique numbers for nodes. */
#endif
};

#ifdef DEBUG_libfirm
/* Returns a new, unique number to number nodes or the like. */
int get_irp_new_node_nr();
#endif

#endif /* ifndef _IRPROG_T_H_ */
