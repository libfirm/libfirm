/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Christian Schaefer
**
** Optimizations for a whole ir graph, i.e., a procedure.
*/

# ifndef _IRGOPT_H_
# define _IRGOPT_H_

# include "irgraph.h"

/* Applies local optimizations (see iropt) to all nodes in the graph. */
void local_optimize_graph (ir_graph *irg);

/* Performs dead node elimination by copying the ir graph to a new obstack. */
void dead_node_elimination(ir_graph *irg);

# endif /* _IRGOPT_H_ */
