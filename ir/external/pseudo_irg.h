
#include "entity.h"
#include "irgraph.h"


/** Create a new ir graph to build a pseudo representation of a procedure.
 *
 *  The pseudo representation can only be used for analyses.  It may not be
 *  optimized.  Pseudo graphs are kept in a separate graph list in irprog.
 */
ir_graph *new_pseudo_ir_graph(entity *ent, int n_loc);

/** Returns true ir ir_graph is pseudo graph.
 *  Is irg a pseudo graph for analysis? */
int      is_pseudo_ir_graph(ir_graph *irg);

/** Returns the number of pseudo graphs in the program. */
int get_irp_n_pseudo_irgs(void);

/** Returns the number of pseudo graphs in the program. */
ir_graph *get_irp_pseudo_irg(int pos);


/** If set, get_irp_n_irgs and get_irp_irg behave as if all
    pseudo graphs are in the irg list. */
void set_visit_pseudo_irgs(int x);
int  get_visit_pseudo_irgs(void);
