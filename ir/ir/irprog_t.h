
# ifndef _IRPROG_T_H_
# define _IRPROG_T_H_

#include "irprog.h"

struct ir_prog {
  firm_kind kind;
  ir_graph  *main_irg;            /* entry point to the compiled program */
                  /* or a list, in case we compile a library or the like? */
  ir_graph **graphs;              /* all graphs in the ir */
  type      *glob_type;           /* global type.  Must be a class as it can
				     have fields and procedures.  */
  type     **types;               /* all types in the ir */
  /*struct obstack *obst;	   * @@@ Should we place all types and
                                     entities on an obstack, too? */
#ifdef DEBUG_libfirm
  long max_node_nr;                /* to generate unique numbers for nodes. */
#endif
};

#endif /* ifndef _IRPROG_T_H_ */
